module WS where

import SharedData

import Prelude

import Data.Array
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
--  let (players :: Array Player) = []
-- TODO: manage players with IORef or similar?
  players <- initPlayers
  let gs = emptyGameState
  wss <- mkServer { port : 8080 }
  -- handler when player connects
  (wss `on` SvConnection) (mkImpureFn1 $ onSocketConnection wss)
  pure unit

foreign import data WS :: Effect

-- TODO: add phantom tags for which message they receive send?
foreign import data Server :: Type -> Type
foreign import data Client :: Type -> Type

mkServer :: ∀ e msg. { port :: Int } -> Eff (ws :: WS | e) (Server msg)
mkServer = unsafeForeignFunction ["config", ""] "new WebSocket.Server(config);"

serverClients :: ∀ e msg. (Server msg) -> Eff (ws :: WS | e) (Array (Client msg))
serverClients = unsafeForeignFunction ["server", ""] "server.clients"

sendMessage :: ∀ e msg. (Encode msg) =>
                   (Client msg) -> msg -> Eff (ws :: WS | e) Unit
sendMessage client msg = unsafeSendMessage client (encodeJSON msg)

unsafeSendMessage :: ∀ e msg.
               (Client msg) -> String -> Eff (ws :: WS | e) Unit
unsafeSendMessage = unsafeForeignProcedure ["client", "data", ""] """
  if (client.readyState === WebSocket.OPEN) {
    client.send(data);
  }
  """

broadcast :: ∀ e msg. (Encode msg) =>
             (Server msg) -> msg -> (Client msg) -> Eff (ws :: WS | e) Unit
broadcast server msg client = do
  clients <- serverClients server
  for_ clients (\c -> sendMessage c msg)

data SvConnection = SvConnection

data ClMessage = ClMessage
data ClClose = ClClose

class WsEvent event where
  eventStr :: event -> String

instance svConnectionEvent :: WsEvent SvConnection where
  eventStr _ = "connection"

instance clMessageEvent :: WsEvent ClMessage where
  eventStr _ = "close"

instance clCloseEvent :: WsEvent ClClose where
  eventStr _ = "close"


class WsListener o msg cb | o -> cb where
  on :: ∀ e a e'. o -> msg -> Impure cb -> Eff (ws :: WS | e') Unit

instance svConnectionListener :: WsListener (Server msg) SvConnection ((Client msg) -> Unit) where
  on = unsafeOn

instance clMessageListener :: WsListener (Client msg) ClMessage (String -> Unit) where
  on = unsafeOn

instance clCloseListener :: WsListener (Client msg) ClClose (Unit -> Unit) where
  on = unsafeOn

unsafeOn :: ∀ o f e msg. (WsEvent msg) => o -> msg -> Impure f -> Eff (ws :: WS | e) Unit
unsafeOn obj msg f = unsafeForeignProcedure ["obj", "event", "cb", ""] "obj.on(event, cb);" obj (eventStr msg) f

onSocketConnection :: ∀ e. (Server ServerMessage) -> (Client ServerMessage) -> Eff (console :: CONSOLE, ws :: WS | e) Unit
onSocketConnection server client = do
  let clientId = 1
  log ("New player has connected: " <> show clientId)
  -- set event handlers
  (client `on` ClMessage) (mkImpureFn1 (onMessage client clientId))
  (client `on` ClClose) (mkImpureFn1 (\_ -> onDisconnect clientId))
  -- send id to client
  sendMessage client (PlayerId { id : clientId })
  -- update other players
  broadcast server (NewPlayer { id : clientId }) client

  players <- getPlayers
  -- send all players to new player
  for_ players (\player -> sendMessage client (NewPlayer { id : player.id }))
  -- update player list
  setPlayers ({id : clientId} : players)

onMessage :: ∀ e. (Client ServerMessage) -> Int -> String -> Eff (console :: CONSOLE | e) Unit
onMessage client id message = do
  log ("received message " <> message)

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
