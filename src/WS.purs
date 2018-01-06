module WS where

import Control.Monad.Eff.Console
import Control.Monad.Eff.Ref
import Data.Array
import Data.Either
import Data.Foreign.Callback
import Data.Foreign.EasyFFI
import Data.Maybe
import Data.Traversable
import Data.Tuple
import Prelude
import SharedData

import ClientMain (gameState)
import Control.Monad.Eff (kind Effect, Eff)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Map as M

type RoomState =
  { players :: Array Player
  , gameState :: SharedGameState
  , playerIdCounter :: Int
  , gidCounter :: Int
  }

emptyRoomState :: RoomState
emptyRoomState = initialRoomState emptyGameState

initialRoomState :: SharedGameState -> RoomState
initialRoomState gs =
  { players : []
  , gameState : gs
  , playerIdCounter : 0
  , gidCounter : 0
  }

pgGameState :: SharedGameState
pgGameState = SharedGameState { cardsByGid : pgCards }

pgCards :: M.Map Int Pack
pgCards = M.fromFoldable ([
  Tuple 0 (Pack { gid : 0
                , cards : cards
                , position : Pos {x : 50, y : 50}
                , lockedBy : Nothing
                , inHandOf : Nothing
                })
  ])
  where
    cards = [ mkCardDown "card" "empty"
            , mkCardDown "card" "empty"
            ]

type Player =
  { id :: PlayerId
  }

startServer :: Eff _ Unit
startServer = do
  log "Server started"
  rsRef <- newRef (initialRoomState pgGameState)
  wss <- mkServer { port : 8080 }
  -- handler when player connects
  (wss `on` SvConnection) (callback1 $ onSocketConnection rsRef wss)
  pure unit

foreign import data WS :: Effect

-- first `Type`: received message types
-- second `Type`: sent message types
-- a server receives cmsg :: client messages
-- a server sends smsg :: server messages
foreign import data Server :: Type -> Type -> Type
-- a server receives smsg :: server messages
-- a server sends cmsg :: client messages
foreign import data Client :: Type -> Type -> Type

foreign import mkServer :: ∀ e cmsg smsg. { port :: Int } -> Eff (ws :: WS | e) (Server cmsg smsg)

serverClients :: ∀ e cmsg smsg. (Server cmsg smsg) -> Eff (ws :: WS | e) (Array (Client smsg cmsg))
serverClients = unsafeForeignFunction ["server", ""] "server.clients"

sendMessage :: ∀ e cmsg smsg. (EncodeJson smsg) =>
               (Client smsg cmsg) -> smsg -> Eff (ws :: WS, console :: CONSOLE | e) Unit
sendMessage client msg = do
  let test = unsafeForeignFunction ["x", ""] "typeof x"
  testVal <- test client
  log ("test_sendMessage: " <> testVal)
  unsafeSendMessage client (stringify $ encodeJson msg)

foreign import unsafeSendMessage :: ∀ e cmsg smsg. (Client cmsg smsg) -> String -> Eff (ws :: WS | e) Unit

broadcast :: ∀ e cmsg smsg. (EncodeJson smsg) =>
             (Server cmsg smsg) -> smsg -> (Client smsg cmsg) -> Eff (ws :: WS, console :: CONSOLE | e) Unit
broadcast server msg exceptClient = unsafeBroadcast server (stringify $ encodeJson msg) exceptClient
--broadcast server msg exceptClient = do
--  clients <- serverClients server
--  for_ clients (\c -> sendMessage c msg)

foreign import unsafeBroadcast :: ∀ e cmsg smsg. (Server cmsg smsg) -> String -> (Client smsg cmsg) -> Eff (ws :: WS | e) Unit


data SvConnection = SvConnection

data ClMessage = ClMessage
data ClClose = ClClose

class WsEvent event where
  eventStr :: event -> String

instance svConnectionEvent :: WsEvent SvConnection where
  eventStr _ = "connection"

instance clMessageEvent :: WsEvent ClMessage where
  eventStr _ = "message"

instance clCloseEvent :: WsEvent ClClose where
  eventStr _ = "close"


class WsListener o msg cb | o -> cb where
  on :: ∀ e a e'. o -> msg -> cb -> Eff (ws :: WS | e') Unit

instance svConnectionListener :: WsListener (Server cmsg smsg) SvConnection (Callback1 (Client smsg cmsg) Unit) where
  on = unsafeOn

instance clMessageListener :: WsListener (Client smsg cmsg) ClMessage (Callback1 String Unit) where
  on = unsafeOn

instance clCloseListener :: WsListener (Client smsg cmsg) ClClose (Callback0 Unit) where
  on = unsafeOn

-- cb is an impure callback
unsafeOn :: ∀ o cb e msg. (WsEvent msg) => o -> msg -> cb -> Eff (ws :: WS | e) Unit
unsafeOn obj msg cb = unsafeForeignProcedure ["obj", "event", "cb", ""] "obj.on(event, cb);" obj (eventStr msg) cb

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
  sendMessage client (ConfirmJoin { assignedId : clientId, roomGameState : roomState.gameState })
  -- update other players
  broadcast server (NewPlayer { id : clientId }) client
  -- send all players to new player
  -- TODO: batch into one message?
  for_ roomState.players (\player -> sendMessage client (NewPlayer { id : player.id }))
  -- update player list
  writeRef rsRef (roomState {players = {id : clientId} : roomState.players, playerIdCounter = clientId})

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
confirmEvent rsRef pid (ClLock gid) = pure $ SvLock gid { pid : pid }
confirmEvent rsRef pid (ClDraw gid { amount : amount }) = do
  rs <- readRef rsRef
  let newGid = rs.gidCounter + 1
  writeRef rsRef (rs {gidCounter = newGid})
  pure $ SvDraw gid { amount, newGid : newGid}
confirmEvent rsRef pid (ClDrop gid pos) = pure $ SvDrop gid pos
confirmEvent rsRef pid (ClDropIn gid x) = pure $ SvDropIn gid x
confirmEvent rsRef pid (ClToHand gid) = pure $ SvToHand gid { pid : pid }

onDisconnect :: Ref RoomState ->
                Int ->
                Eff _ Unit
onDisconnect rsRef toRemoveId = do
  -- find player to remove
  log ("removing player")
  roomState <- readRef rsRef
  let (newPlayers :: Array Player) = filter (\p -> p.id /= toRemoveId) roomState.players
  writeRef rsRef (roomState {players = newPlayers})
