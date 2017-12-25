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
import Data.Map as M
import Data.Maybe
import Data.Traversable
import Data.Tuple

import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Console
import Control.Monad.Eff.Ref

foreign import initServerState :: ∀ e. Eff (ws :: WS | e) RoomState
foreign import getServerState :: ∀ e. Eff (ws :: WS | e) RoomState
foreign import setServerState :: ∀ e. RoomState -> Eff (ws :: WS | e) Unit

type RoomState =
  { players :: Array Player
  , gameState :: SharedGameState
  }

emptyRoomState :: RoomState
emptyRoomState = initialRoomState emptyGameState

initialRoomState :: SharedGameState -> RoomState
initialRoomState gs =
  { players : []
  , gameState : gs
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
-- TODO: manage serverState with IORef or similar?
  setServerState (initialRoomState pgGameState)
  wss <- mkServer { port : 8080 }
  -- handler when player connects
  (wss `on` SvConnection) (mkImpureFn1 $ onSocketConnection wss)
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
  on :: ∀ e a e'. o -> msg -> Impure cb -> Eff (ws :: WS | e') Unit

instance svConnectionListener :: WsListener (Server cmsg smsg) SvConnection ((Client smsg cmsg) -> Unit) where
  on = unsafeOn

instance clMessageListener :: WsListener (Client smsg cmsg) ClMessage (String -> Unit) where
  on = unsafeOn

instance clCloseListener :: WsListener (Client smsg cmsg) ClClose (Unit -> Unit) where
  on = unsafeOn

unsafeOn :: ∀ o f e msg. (WsEvent msg) => o -> msg -> Impure f -> Eff (ws :: WS | e) Unit
unsafeOn obj msg f = unsafeForeignProcedure ["obj", "event", "cb", ""] "obj.on(event, cb);" obj (eventStr msg) f

onSocketConnection :: ∀ e.
                      (Server ClientMessage ServerMessage) ->
                      (Client ServerMessage ClientMessage) ->
                      Eff (console :: CONSOLE, ws :: WS | e) Unit
onSocketConnection server client = do
  -- TODO: generate unique id
  let clientId = 1
  log ("New player has connected: " <> show clientId)
  -- set event handlers
  (client `on` ClMessage) (mkImpureFn1 (onMessage server client clientId))
  (client `on` ClClose) (mkImpureFn1 (\_ -> onDisconnect clientId))
  -- read room state
  roomState <- getServerState
  -- send id to client
  -- TODO: put actual gamestate
  sendMessage client (ConfirmJoin { assignedId : clientId, roomGameState : roomState.gameState })
  -- update other players
  broadcast server (NewPlayer { id : clientId }) client

  -- send all players to new player
  -- TODO: batch into one message?
  for_ roomState.players (\player -> sendMessage client (NewPlayer { id : player.id }))
  -- update player list
  setServerState (roomState {players = {id : clientId} : roomState.players})

onMessage :: ∀ e.
             (Server ClientMessage ServerMessage) ->
             (Client ServerMessage ClientMessage) ->
             Int -> String -> Eff (console :: CONSOLE, ws :: WS | e) Unit
onMessage server client id message = do
  log ("received message " <> message)
  let eSvMsg = jsonParser message >>= decodeJson
  case eSvMsg of
    Left errs -> log ("malformed message")
    Right (clMsg :: ClientMessage) -> onClientMessage server client id clMsg

onClientMessage :: ∀ e.
                   (Server ClientMessage ServerMessage) ->
                   (Client ServerMessage ClientMessage) ->
                   Int -> ClientMessage -> Eff (console :: CONSOLE, ws :: WS | e) Unit
onClientMessage server client clientId (ClMoveGid {id: playerId, x: x, y: y}) = do
  -- check if client id == player id?
  log ("player " <> show playerId <> " moving gid, x:" <> show x <> ", y: " <> show y)
  broadcast server (SvMoveGid {id :playerId, x: x, y: y}) client
onClientMessage server client clientId (ClGameStateUpdate {events: events}) = do
  log ("events: " <> show events)
  sendMessage client (ConfirmUpdates {events: events})

onDisconnect :: ∀ e. Int -> Eff (console :: CONSOLE, ws :: WS | e) Unit
onDisconnect toRemoveId = do
  -- find player to remove
  log ("removing player")
  roomState <- getServerState
  let (newPlayers :: Array Player) = filter (\p -> p.id /= toRemoveId) roomState.players
  setServerState (roomState {players = newPlayers})

-- TODO: use purescript-foreign-callbacks? (needs to be updated first)

newtype Impure f = Impure f

mkImpureFn1 :: ∀ a e r. (a -> Eff e r) -> Impure (a -> r)
mkImpureFn1 f = Impure (unsafeForeignFunction ["f", "a"] "f(a)();" f)

mkImpureFn2 :: ∀ a b e r. (a -> b -> Eff e r) -> Impure (a -> b -> r)
mkImpureFn2 f = Impure (unsafeForeignFunction ["f", "a", "b"] "f(a)(b)();" f)