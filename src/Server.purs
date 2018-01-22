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


startServer :: forall f server client r ref.
               (Monad f) =>
               (MonadRef ref f) =>
               { log :: String -> f Unit
               , initializeRooms :: f (ref RoomState)
               , initializeServer :: f server
               , onClConnect :: (client -> f Unit) -> server -> f Unit
               , onClMessage :: (String -> f Unit) -> client -> f Unit
               , onClDisconnect :: (f Unit) -> client -> f Unit
               , sendMessage :: ServerMessage -> client -> f Unit
               , broadcast :: ServerMessage -> { except :: client } -> server -> f Unit
               | r } ->
               f Unit
startServer k = do
  k.log "Initializing Rooms"
  roomStates <- k.initializeRooms
  k.log "Starting Server"
  server <- k.initializeServer
  server # k.onClConnect (onSocketConnection k roomStates server)

onSocketConnection :: forall f server client ref r msg1 msg2 f1 f2.
                      (Monad f) =>
                      (MonadRef ref f) =>
                      { log :: String -> f Unit
                      , onClMessage :: (String -> f Unit) -> client -> f Unit
                      , onClDisconnect :: (f Unit) -> client -> f Unit
                      , sendMessage :: ServerMessage -> client -> f Unit
                      , broadcast :: ServerMessage -> { except :: client } -> server -> f Unit
                      | r } ->
                      ref RoomState ->
                      server ->
                      client ->
                      f Unit
onSocketConnection k rsRef server client = do
  -- read roomState
  roomState <- readRef rsRef
  let newPlayerId = roomState.playerIdCounter + 1
  k.log ("New player has connected: " <> show newPlayerId)
  -- set event handlers
  client # k.onClMessage (onMessage k rsRef server client newPlayerId)
  client # k.onClDisconnect (onDisconnect k rsRef newPlayerId)
  -- send id to client
  client # k.sendMessage (ConfirmJoin { assignedId: newPlayerId, roomGameState: roomState.gameState })
  -- update other players
  server # k.broadcast (NewPlayer { id: newPlayerId }) { except: client }
  -- send all players to new player
  -- TODO: batch into one message?
  for_ roomState.players (\player -> client # k.sendMessage (NewPlayer { id: player.id }))
  -- update player list
  writeRef rsRef (roomState { players= {id: newPlayerId} : roomState.players, playerIdCounter= newPlayerId})

onMessage :: forall f ref server client r.
             (Monad f) =>
             (MonadRef ref f) =>
             { log :: String -> f Unit
             , broadcast :: ServerMessage -> { except :: client } -> server -> f Unit
             , sendMessage :: ServerMessage -> client -> f Unit
             | r } ->
             ref RoomState ->
             server ->
             client ->
             Int ->
             String ->
             f Unit
onMessage k rsRef server client id message = do
  k.log ("received message " <> message)
  case jsonParser message >>= decodeJson of
    Left (errs :: String) ->
      k.log ("malformed message: " <> errs)
    Right (clMsg :: ClientMessage) ->
      onClientMessage k rsRef server client id clMsg

onClientMessage :: forall f ref server client r.
                   (Monad f) =>
                   (MonadRef ref f) =>
                   { log :: String -> f Unit
                   , broadcast :: ServerMessage -> { except :: client } -> server -> f Unit
                   , sendMessage :: ServerMessage -> client -> f Unit
                   | r } ->
                   ref RoomState ->
                   server ->
                   client ->
                   Int ->
                   ClientMessage ->
                   f Unit
onClientMessage k rsRef server client clientId (ClMoveGid {id: playerId, x: x, y: y}) = do
  -- check if client id == player id?
  k.log ("player " <> show playerId <> " moving gid, x:" <> show x <> ", y: " <> show y)
  server # k.broadcast (SvMoveGid { id: playerId, x: x, y: y }) { except: client }
  -- TODO: update gid position in server gamestate?
onClientMessage k rsRef server client clientId (ClGameStateUpdate {events: events}) = do
  k.log ("events: " <> show events)
  confirmedEvents <- traverse (confirmEvent k rsRef clientId) events
  roomState <- readRef rsRef
  let updatedGameState = foldr updateSharedGameState roomState.gameState confirmedEvents
  writeRef rsRef (roomState { gameState= updatedGameState })
  client # k.sendMessage (ConfirmUpdates { events: confirmedEvents })
  server # k.broadcast (ConfirmUpdates { events: confirmedEvents }) { except: client }

confirmEvent :: forall f ref r.
                (Monad f) =>
                (MonadRef ref f) =>
                { log :: String -> f Unit
                | r } ->
                ref RoomState ->
                PlayerId ->
                ClGameEvent ->
                f SvGameEvent
confirmEvent k rsRef pid (ClSelect gid) = pure $ SvSelect gid
confirmEvent k rsRef pid (ClGather) = pure $ SvGather
confirmEvent k rsRef pid (ClRemove gid) = pure $ SvRemove gid
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

{-
startServer :: Eff _ Unit
startServer = do
  log "Reading Predefined Cards"
  ahBaseWhite <- parseFile "server_assets/ah/base/ah_white.txt"
  ahBaseBlack <- parseFile "server_assets/ah/base/ah_black.txt"
  log "Initializing Rooms"
  rsRef <- newRef (initialRoomState [ahBaseWhite, ahBaseBlack])
  log "Starting Server"
  wss <- mkServer { port: 8080 }
  -- handler when player connects
  wss # on SvConnection (callback1 $ onSocketConnection rsRef wss)
  pure unit
    where
      parseFile path = do
        content <- readTextFile UTF8 path
        pure $ content # split (Pattern "\n")
                       # filter (\x -> x /= "")

onSocketConnection :: Ref RoomState ->
                      (Server ClientMessage ServerMessage) ->
                      (Client ServerMessage ClientMessage) ->
                      Eff _ Unit
onSocketConnection rsRef server client = do
  -- read roomState
  roomState <- readRef rsRef
  let clientId = roomState.playerIdCounter + 1
  log ("New player has connected: " <> show clientId)
  -- set event handlers
  client # on ClMessage (callback1 $ onMessage rsRef server client clientId)
  client # on ClClose (callback0 $ onDisconnect rsRef clientId)
  -- send id to client
  sendMessage client (ConfirmJoin { assignedId: clientId, roomGameState: roomState.gameState })
  -- update other players
  broadcast server (NewPlayer { id: clientId }) { except: client }
  -- send all players to new player
  -- TODO: batch into one message?
  for_ roomState.players (\player -> sendMessage client (NewPlayer { id: player.id }))
  -- update player list
  writeRef rsRef (roomState { players= {id: clientId} : roomState.players, playerIdCounter= clientId})

onMessage :: Ref RoomState ->
             (Server ClientMessage ServerMessage) ->
             (Client ServerMessage ClientMessage) ->
             Int ->
             String ->
             Eff _ Unit
onMessage rsRef server client id message = do
  log ("received message " <> message)
  case jsonParser message >>= decodeJson of
    Left (errs :: String) ->
      log ("malformed message: " <> errs)
    Right (clMsg :: ClientMessage) ->
      onClientMessage rsRef server client id clMsg

onClientMessage :: Ref RoomState ->
                   (Server ClientMessage ServerMessage) ->
                   (Client ServerMessage ClientMessage) ->
                   Int ->
                   ClientMessage ->
                   Eff _ Unit
onClientMessage rsRef server client clientId (ClMoveGid {id: playerId, x: x, y: y}) = do
  -- check if client id == player id?
  log ("player " <> show playerId <> " moving gid, x:" <> show x <> ", y: " <> show y)
  broadcast server (SvMoveGid { id: playerId, x: x, y: y }) { except: client }
  -- TODO: update gid position in server gamestate?
onClientMessage rsRef server client clientId (ClGameStateUpdate {events: events}) = do
  log ("events: " <> show events)
  confirmedEvents <- traverse (confirmEvent rsRef clientId) events
  roomState <- readRef rsRef
  let updatedGameState = foldr updateSharedGameState roomState.gameState confirmedEvents
  writeRef rsRef (roomState { gameState= updatedGameState })
  sendMessage client (ConfirmUpdates { events: confirmedEvents })
  broadcast server (ConfirmUpdates { events: confirmedEvents }) { except: client }

confirmEvent :: Ref RoomState ->
                PlayerId ->
                ClGameEvent ->
                Eff _ SvGameEvent
confirmEvent rsRef pid (ClSelect gid) = pure $ SvSelect gid
confirmEvent rsRef pid (ClGather) = pure $ SvGather
confirmEvent rsRef pid (ClRemove gid) = pure $ SvRemove gid
confirmEvent rsRef pid (ClFlip gid) = pure $ SvFlip gid
confirmEvent rsRef pid (ClLock gid) = do
  rs <- readRef rsRef
  let gidLockedBy = forGid gid (\(Pack p) -> p.lockedBy) rs.gameState
  log ("locked: " <> show gidLockedBy)
  pure $ case gidLockedBy of
    -- card is locked by player
    Just (Just _) -> SvLockDeny
    -- card is not locked
    Just Nothing -> SvLock gid { pid: pid }
    -- card does not exist on server
    Nothing -> SvLockDeny
confirmEvent rsRef pid (ClDraw gid { amount: amount }) = do
  rs <- readRef rsRef
  let gidLockedBy = forGid gid (\(Pack p) -> p.lockedBy) rs.gameState
  log ("locked: " <> show gidLockedBy)
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
confirmEvent rsRef pid (ClDrop gid pos) = pure $ SvDrop gid pos
confirmEvent rsRef pid (ClDropIn gid x) = pure $ SvDropIn gid x
confirmEvent rsRef pid (ClToHand gid) = pure $ SvToHand gid { pid: pid }

onDisconnect :: Ref RoomState ->
                Int ->
                Eff _ Unit
onDisconnect rsRef toRemoveId = do
  -- find player to remove
  log ("removing player")
  -- remove player from room
  roomState <- readRef rsRef
  let (newPlayers :: Array Player) = filter (\p -> p.id /= toRemoveId) roomState.players
  -- place in hand cards back on field (TODO: only after delay, to allow reconnect?)
  let (newGS :: SharedGameState) = mapCards (dropPack toRemoveId) roomState.gameState
  -- update ref
  writeRef rsRef (roomState {players= newPlayers, gameState= newGS})
-}

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
