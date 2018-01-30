module Server where

import Types
import SharedData
import Pack
import GameState
import WS

import Data.Array (filter, mapWithIndex, (:))
import Data.Foldable (length)
import Data.Foreign.Callback
import Data.List (List(..), catMaybes)
import Data.Map as M
import Data.String (Pattern(..), split)
import Data.Traversable

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)

import Control.Monad.Eff.Console
--import Control.Monad.Eff.Ref
import MonadRef

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

type RoomState =
  { players :: Array Player
  , gameState :: SharedGameState
  , playerIdCounter :: Int
  , gidCounter :: Int
  }

onSocketConnection :: forall f client ref r msg1 msg2 f1 f2.
                      (Monad f) =>
                      (MonadRef ref f) =>
                      { log :: String -> f Unit
                      , onClMessage :: (String -> f Unit) -> client -> f Unit
                      , onClDisconnect :: (f Unit) -> client -> f Unit
                      , sendMessage :: ServerMessage -> client -> f Unit
                      , broadcast :: ServerMessage -> { except :: client } -> f Unit
                      | r } ->
                      ref RoomState ->
                      client ->
                      f Unit
onSocketConnection k rsRef client = do
  -- read roomState
  roomState <- readRef rsRef
  let newPlayerId = roomState.playerIdCounter + 1
  k.log ("New player has connected: " <> show newPlayerId)
  -- set event handlers
  client # k.onClMessage (onMessage k rsRef client newPlayerId)
  client # k.onClDisconnect (onDisconnect k rsRef newPlayerId)
  -- send id to client
  client # k.sendMessage (ConfirmJoin { assignedId: newPlayerId, roomGameState: roomState.gameState })
  -- update other players
  { except: client } # k.broadcast (NewPlayer { id: newPlayerId })
  -- send all players to new player
  -- TODO: batch into one message?
  for_ roomState.players (\player -> client # k.sendMessage (NewPlayer { id: player.id }))
  -- update player list
  writeRef rsRef (roomState { players= {id: newPlayerId} : roomState.players, playerIdCounter= newPlayerId})

onMessage :: forall f ref client r.
             (Monad f) =>
             (MonadRef ref f) =>
             { log :: String -> f Unit
             , broadcast :: ServerMessage -> { except :: client } -> f Unit
             , sendMessage :: ServerMessage -> client -> f Unit
             | r } ->
             ref RoomState ->
             client ->
             Int ->
             String ->
             f Unit
onMessage k rsRef client id message = do
  k.log ("received message " <> message)
  case jsonParser message >>= decodeJson of
    Left (errs :: String) ->
      k.log ("malformed message: " <> errs)
    Right (clMsg :: ClientMessage) ->
      onClientMessage k rsRef client id clMsg

onClientMessage :: forall f ref client r.
                   (Monad f) =>
                   (MonadRef ref f) =>
                   { log :: String -> f Unit
                   , broadcast :: ServerMessage -> { except :: client } -> f Unit
                   , sendMessage :: ServerMessage -> client -> f Unit
                   | r } ->
                   ref RoomState ->
                   client ->
                   Int ->
                   ClientMessage ->
                   f Unit
onClientMessage k rsRef client clientId (ClMoveGid {id: playerId, x: x, y: y}) = do
  -- check if client id == player id?
  k.log ("player " <> show playerId <> " moving gid, x:" <> show x <> ", y: " <> show y)
  { except: client } # k.broadcast (SvMoveGid { id: playerId, x: x, y: y })
  -- TODO: update gid position in server gamestate?
onClientMessage k rsRef client clientId (ClGameStateUpdate {events: events}) = do
  k.log ("events: " <> show events)
  confirmedEvents <- traverse (confirmEvent k rsRef clientId) events
  roomState <- readRef rsRef
  let updatedGameState = foldr updateSharedGameState roomState.gameState confirmedEvents
  writeRef rsRef (roomState { gameState= updatedGameState })
  client # k.sendMessage (ConfirmUpdates { events: confirmedEvents })
  { except: client } # k.broadcast (ConfirmUpdates { events: confirmedEvents })

confirmEvent :: forall f ref r.
                (Monad f) =>
                (MonadRef ref f) =>
                { log :: String -> f Unit
                | r } ->
                ref RoomState ->
                PlayerId ->
                ClGameEvent ->
                f SvGameEvent
confirmEvent k rsRef pid (ClFlip gid) = pure $ SvFlip gid
confirmEvent k rsRef pid (ClLock gid) = do
  rs <- readRef rsRef
  let gidLockedBy = forGid gid (\(Pack p) -> p.lockedBy) rs.gameState
  k.log ("locked: " <> show gidLockedBy)
  pure $ case gidLockedBy of
    -- card is locked by player
    Just (Just _) -> SvLockDeny
    -- card is not locked
    Just Nothing -> SvLock gid { pid: pid }
    -- card does not exist on server
    Nothing -> SvLockDeny
confirmEvent k rsRef pid (ClDraw gid { amount: amount }) = do
  rs <- readRef rsRef
  let gidLockedBy = forGid gid (\(Pack p) -> p.lockedBy) rs.gameState
  k.log ("locked: " <> show gidLockedBy)
  case gidLockedBy of
    -- card is locked by player
    Just (Just _) -> pure $ SvLockDeny
    -- card is not locked
    Just Nothing -> do
      let newGid = rs.gidCounter + 1
      writeRef rsRef (rs {gidCounter = newGid})
      pure $ SvDraw gid { amount, newGid: newGid}
    -- card does not exist on server
    Nothing -> pure $ SvLockDeny
confirmEvent k rsRef pid (ClDrop gid pos) = pure $ SvDrop gid pos
confirmEvent k rsRef pid (ClDropIn gid x) = pure $ SvDropIn gid x
confirmEvent k rsRef pid (ClToHand gid) = pure $ SvToHand gid { pid: pid }
confirmEvent k rsRef pid (ClShuffle gid) = do
  rs <- readRef rsRef
  let gidLockedBy = forGid gid (\(Pack p) -> p.lockedBy) rs.gameState
  k.log ("locked: " <> show gidLockedBy)
  case gidLockedBy of
    -- card is locked by player
    Just (Just _) -> pure $ SvActionDeny gid
    -- card is not locked
    Just Nothing -> do
      pure $ SvShuffle gid { seed: "1" }
    -- card does not exist on server
    Nothing -> pure $ SvActionDeny gid

onDisconnect :: forall f ref r.
                (Monad f) =>
                (MonadRef ref f) =>
                { log :: String -> f Unit
                | r } ->
                ref RoomState ->
                Int ->
                f Unit
onDisconnect k rsRef toRemoveId = do
  -- find player to remove
  k.log ("removing player")
  -- remove player from room
  roomState <- readRef rsRef
  let (newPlayers :: Array Player) = filter (\p -> p.id /= toRemoveId) roomState.players
  -- place in hand cards back on field (TODO: only after delay, to allow reconnect?)
  let (newGS :: SharedGameState) = mapCards (dropPack toRemoveId) roomState.gameState
  -- update ref
  writeRef rsRef (roomState {players= newPlayers, gameState= newGS})

initRooms = do
  log "Reading Predefined Cards"
  ahBaseWhite <- parseFile "server_assets/ah/base/ah_white.txt"
  ahBaseBlack <- parseFile "server_assets/ah/base/ah_black.txt"
  newRef (initialRoomState [ahBaseWhite, ahBaseBlack])
    where
      parseFile path = do
        content <- readTextFile UTF8 path
        pure $ content # split (Pattern "\n")
                       # filter (\x -> x /= "")

-- helper functions for initial state

emptyRoomState :: RoomState
emptyRoomState = initialRoomState [[]]

initialRoomState :: Array (Array String) -> RoomState
initialRoomState packs =
  { players: []
  , gameState: pgGameState packs
  , playerIdCounter: 0
  , gidCounter: length packs
  }

pgGameState :: Array (Array String) -> SharedGameState
pgGameState packs = SharedGameState { cardsByGid: pgCards packs }

pgCards :: Array (Array String) -> M.Map Int Pack
pgCards packs = M.fromFoldable (mapWithIndex createPack packs)
  where
    createPack i texts = Tuple i $ Pack
      { gid: i
      , cards: cards texts
      , position: OnBoard {x: 50 + 50 * i, y: 50}
      , lockedBy: Nothing
      }
    cards texts = texts <#> (\x -> mkCardDown "card" "empty" x)
