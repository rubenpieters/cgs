module WS where

import SharedData

import Prelude

import Data.Array
import Data.Either
import Data.Foreign.Class (class Encode, class Decode)
import Data.Foreign.EasyFFI
import Data.Foreign.Generic (encodeJSON, decodeJSON)
import Data.Maybe
import Data.Traversable

import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Console

foreign import initPlayers :: ∀ e. Eff (ws :: WS | e) (Array Player)
foreign import getPlayers :: ∀ e. Eff (ws :: WS | e) (Array Player)
foreign import setPlayers :: ∀ e. Array Player -> Eff (ws :: WS | e) Unit

type Player =
  { id :: Int
  }

startServer = do
  log "Server started"
-- TODO: manage players with IORef or similar?
  players <- initPlayers
  let gs = emptyGameState
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

sendMessage :: ∀ e cmsg smsg. (Encode smsg) =>
               (Client smsg cmsg) -> smsg -> Eff (ws :: WS, console :: CONSOLE | e) Unit
sendMessage client msg = do
  let test = unsafeForeignFunction ["x", ""] "typeof x"
  testVal <- test client
  log ("test_sendMessage: " <> testVal)
  unsafeSendMessage client (encodeJSON msg)

foreign import unsafeSendMessage :: ∀ e cmsg smsg. (Client cmsg smsg) -> String -> Eff (ws :: WS | e) Unit

broadcast :: ∀ e cmsg smsg. (Encode smsg) =>
             (Server cmsg smsg) -> smsg -> (Client smsg cmsg) -> Eff (ws :: WS, console :: CONSOLE | e) Unit
broadcast server msg exceptClient = unsafeBroadcast server (encodeJSON msg) exceptClient
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
  -- send id to client
  -- TODO: put actual gamestate
  sendMessage client (ConfirmJoin { assignedId : clientId, serverGameState : emptyGameState })
  -- update other players
  broadcast server (NewPlayer { id : clientId }) client

  players <- getPlayers
  -- send all players to new player
  -- TODO: batch into one message?
  for_ players (\player -> sendMessage client (NewPlayer { id : player.id }))
  -- update player list
  setPlayers ({id : clientId} : players)

onMessage :: ∀ e.
             (Server ClientMessage ServerMessage) ->
             (Client ServerMessage ClientMessage) ->
             Int -> String -> Eff (console :: CONSOLE, ws :: WS | e) Unit
onMessage server client id message = do
  log ("received message " <> message)
  let eSvMsg = decodeJSONEither message
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
  players <- getPlayers
  let newPlayers = filter (\p -> p.id /= toRemoveId) players
  setPlayers newPlayers

-- TODO: use purescript-foreign-callbacks? (needs to be updated first)

newtype Impure f = Impure f

mkImpureFn1 :: ∀ a e r. (a -> Eff e r) -> Impure (a -> r)
mkImpureFn1 f = Impure (unsafeForeignFunction ["f", "a"] "f(a)();" f)

mkImpureFn2 :: ∀ a b e r. (a -> b -> Eff e r) -> Impure (a -> b -> r)
mkImpureFn2 f = Impure (unsafeForeignFunction ["f", "a", "b"] "f(a)(b)();" f)
