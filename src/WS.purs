module WS where

import SharedData

import Prelude
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array
import Data.Either
import Data.Foreign.EasyFFI
import Data.Foreign.Callback
import Data.Map as M
import Data.Maybe
import Data.Traversable
import Data.Tuple

import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Console
import Control.Monad.Eff.Ref

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
onClientMessage rsRef server client clientId (ClGameStateUpdate {events: events}) = do
  log ("events: " <> show events)
  confirmedEvents <- traverse (confirmEvent rsRef) events
  sendMessage client (ConfirmUpdates {events: confirmedEvents})

confirmEvent :: Ref RoomState ->
                ClGameEvent ->
                Eff _ SvGameEvent
confirmEvent rsRef (ClSelect gid) = pure $ SvSelect gid
confirmEvent rsRef (ClGather) = pure $ SvGather
confirmEvent rsRef (ClRemove gid) = pure $ SvRemove gid
confirmEvent rsRef (ClFlip gid) = pure $ SvFlip gid
confirmEvent rsRef (ClLock gid x) = pure $ SvLock gid x
confirmEvent rsRef (ClDraw gid { amount : amount }) = do
  rs <- readRef rsRef
  let newGid = rs.gidCounter + 1
  writeRef rsRef (rs {gidCounter = newGid})
  pure $ SvDraw gid { amount, newGid : newGid}
confirmEvent rsRef (ClDrop gid pos) = pure $ SvDrop gid pos
confirmEvent rsRef (ClDropIn gid x) = pure $ SvDropIn gid x
confirmEvent rsRef (ClToHand gid x) = pure $ SvToHand gid x

onDisconnect :: Ref RoomState ->
                Int ->
                Eff _ Unit
onDisconnect rsRef toRemoveId = do
  -- find player to remove
  log ("removing player")
  roomState <- readRef rsRef
  let (newPlayers :: Array Player) = filter (\p -> p.id /= toRemoveId) roomState.players
  writeRef rsRef (roomState {players = newPlayers})
