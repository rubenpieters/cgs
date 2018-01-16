module WS where

import Types
import SharedData
import Pack
import ClientMain (gameState)
import GameState

import Data.Array hiding (length, catMaybes)
import Data.List (List(..), catMaybes)
import Data.Foreign.Callback
import Data.Foreign.EasyFFI
import Data.Traversable
import Data.Foldable
import Data.Map as M

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)

import Control.Monad.Eff.Console
import Control.Monad.Eff.Ref

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
             (Server cmsg smsg) -> smsg -> {except :: (Client smsg cmsg)} -> Eff (ws :: WS, console :: CONSOLE | e) Unit
broadcast server msg {except: exceptClient} = unsafeBroadcast server (stringify $ encodeJson msg) exceptClient
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
  on :: ∀ e a e'. msg -> cb -> o -> Eff (ws :: WS | e') Unit

instance svConnectionListener :: WsListener (Server cmsg smsg) SvConnection (Callback1 (Client smsg cmsg) Unit) where
  on = unsafeOn

instance clMessageListener :: WsListener (Client smsg cmsg) ClMessage (Callback1 String Unit) where
  on = unsafeOn

instance clCloseListener :: WsListener (Client smsg cmsg) ClClose (Callback0 Unit) where
  on = unsafeOn

-- cb is an impure callback
unsafeOn :: ∀ o cb e msg. (WsEvent msg) => msg -> cb -> o -> Eff (ws :: WS | e) Unit
unsafeOn msg cb obj = unsafeForeignProcedure ["event", "cb","obj", ""] "obj.on(event, cb);" (eventStr msg) cb obj

