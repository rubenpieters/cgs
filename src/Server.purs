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
import Control.Monad.Eff.Ref

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

type RoomState =
  { players :: Array Player
  , gameState :: SharedGameState
  , playerIdCounter :: Int
  , gidCounter :: Int
  }

startServer :: Eff _ Unit
startServer = do
  log "Reading Predefined Cards"
  ahBaseWhite <- (readTextFile UTF8 "server_assets/ah/base/ah_white.txt") <#> split (Pattern "\n") <#> filter (\x -> x /= "")
  ahBaseBlack <- (readTextFile UTF8 "server_assets/ah/base/ah_black.txt") <#> split (Pattern "\n") <#> filter (\x -> x /= "")
  log "Initialzing Rooms"
  rsRef <- newRef (initialRoomState [ahBaseWhite, ahBaseBlack])
  log "Starting Server"
  wss <- mkServer { port : 8080 }
  -- handler when player connects
  (wss `on` SvConnection) (callback1 $ onSocketConnection rsRef wss)
  pure unit

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
  (client `on` ClMessage) (callback1 $ onMessage rsRef server client clientId)
  (client `on` ClClose) (callback0 $ onDisconnect rsRef clientId)
  -- send id to client
  sendMessage client (ConfirmJoin { assignedId: clientId, roomGameState : roomState.gameState })
  -- update other players
  broadcast server (NewPlayer { id: clientId }) client
  -- send all players to new player
  -- TODO: batch into one message?
  for_ roomState.players (\player -> sendMessage client (NewPlayer { id : player.id }))
  -- update player list
  writeRef rsRef (roomState {players = {id: clientId} : roomState.players, playerIdCounter= clientId})

onMessage :: Ref RoomState ->
             (Server ClientMessage ServerMessage) ->
             (Client ServerMessage ClientMessage) ->
             Int ->
             String ->
             Eff _ Unit
onMessage rsRef server client id message = do
  log ("received message " <> message)
  let eSvMsg = jsonParser message >>= decodeJson
  case eSvMsg of
    Left errs -> log ("malformed message")
    Right (clMsg :: ClientMessage) -> onClientMessage rsRef server client id clMsg

onClientMessage :: Ref RoomState ->
                   (Server ClientMessage ServerMessage) ->
                   (Client ServerMessage ClientMessage) ->
                   Int ->
                   ClientMessage ->
                   Eff _ Unit
onClientMessage rsRef server client clientId (ClMoveGid {id: playerId, x: x, y: y}) = do
  -- check if client id == player id?
  log ("player " <> show playerId <> " moving gid, x:" <> show x <> ", y: " <> show y)
  broadcast server (SvMoveGid {id :playerId, x: x, y: y}) client
  -- TODO: update gid position in server gamestate?
onClientMessage rsRef server client clientId (ClGameStateUpdate {events: events}) = do
  log ("events: " <> show events)
  confirmedEvents <- traverse (confirmEvent rsRef clientId) events
  roomState <- readRef rsRef
  let updatedGameState = foldr updateSharedGameState roomState.gameState confirmedEvents
  writeRef rsRef (roomState { gameState= updatedGameState })
  sendMessage client (ConfirmUpdates {events: confirmedEvents})
  broadcast server (ConfirmUpdates {events: confirmedEvents}) client

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
    Just Nothing -> SvLock gid { pid : pid }
    -- card does not exist on server
    Nothing -> SvLockDeny
confirmEvent rsRef pid (ClDraw gid { amount : amount }) = do
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
      pure $ SvDraw gid { amount, newGid : newGid}
    -- card does not exist on server
    Nothing -> pure $ SvLockDeny
confirmEvent rsRef pid (ClDrop gid pos) = pure $ SvDrop gid pos
confirmEvent rsRef pid (ClDropIn gid x) = pure $ SvDropIn gid x
confirmEvent rsRef pid (ClToHand gid) = pure $ SvToHand gid { pid : pid }

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

dropPack :: PlayerId -> Pack -> Pack
dropPack pid (Pack p) = case Tuple p.position p.lockedBy of
  Tuple (InHandOf {pid: pid'}) _ | pid == pid' ->
    Pack $ p {position= OnBoard {x: 0, y: 0}, lockedBy= Nothing}
  Tuple _ (Just pid') | pid == pid' ->
    Pack $ p {position= OnBoard {x: 0, y: 0}, lockedBy= Nothing}
  _ ->
    Pack p

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
pgGameState packs = SharedGameState { cardsByGid : pgCards packs }

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
