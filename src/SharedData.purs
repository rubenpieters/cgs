module SharedData where

import Types
import Pack
import GameState

import Data.Array
import Data.Foldable
import Data.Foreign
import Data.Traversable
import Data.Unfoldable
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep as Rep
import Data.Generic.Rep.Show (genericShow)
import Data.Map as M

import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrowException)

type Player =
  { id :: Int
  }

type ObfuscatedGameState = SharedGameState

updateSharedGameState :: SvGameEvent -> SharedGameState -> SharedGameState
updateSharedGameState (SvSelect gid) gs = gs
updateSharedGameState SvGather gs = gs
updateSharedGameState (SvRemove gid) gs = gs
updateSharedGameState (SvFlip gid) gs = onGid gid flipTop gs
updateSharedGameState (SvLock gid { pid: pid }) gs = onGid gid (lockPack pid) gs
updateSharedGameState (SvLockDeny) gs = gs
updateSharedGameState (SvDraw gid { amount: amount, newGid: newGid }) gs =
  case (forGid gid (drawFromPack amount) gs) of
    Just { remaining: remaining, drawn: drawn } ->
      -- set cards of old pack to remaining cards
      let gs' = onGid gid (\(Pack p) -> Pack $ p { cards= remaining }) gs
      -- create new pack from drawn cards
          newPack = Pack { position: (OnBoard { x: 0, y: 0 })
                         , gid: newGid
                         , cards: drawn
                         , lockedBy: Nothing }
      in setGid newGid newPack gs'
    Nothing -> gs
--    where { remaining: remaining, drawn: drawn } = drawFromPack (gs.cardsById)
updateSharedGameState (SvDrop gid pos) gs = onGid gid (dropAt pos >>> unlockPack) gs
updateSharedGameState (SvDropIn drp { tgt: pk }) gs =
  case (forGid drp (\(Pack p) -> p.cards) gs) of
    Just (cards :: Array Card) ->
      let gs' = onGid pk (\(Pack p) -> Pack $ p { cards= p.cards <> cards }) gs
      in removeGid drp gs'
    Nothing -> gs
updateSharedGameState (SvToHand gid { pid: pid }) gs = onGid gid (packToHand pid) gs

-- CLIENT GAME EVENT
-- client sends to server

data ClGameEvent = ClSelect Gid
                 | ClGather
                 | ClRemove Gid
                 | ClFlip Gid
                 | ClLock Gid
                 | ClDraw Gid { amount :: Int }
                 | ClDrop Gid { x :: Int, y :: Int }
                 | ClDropIn Gid { tgt :: Gid }
                 | ClToHand Gid

-- SERVER GAME EVENT
-- server sends to client
-- server adds information where necessary

data SvGameEvent = SvSelect Gid
                 | SvGather
                 | SvRemove Gid
                 | SvFlip Gid
                 | SvLock Gid { pid :: Int }
                 | SvLockDeny
                 | SvDraw Gid { amount :: Int, newGid :: Int }
                 | SvDrop Gid { x :: Int, y :: Int }
                 | SvDropIn Gid { tgt :: Gid }
                 | SvToHand Gid { pid :: Int }

-- messages server -> client
data ServerMessage
  = ConfirmJoin { assignedId :: Int, roomGameState :: ObfuscatedGameState }
  | NewPlayer { id :: Int }
  | SvMoveGid { id :: Int, x :: Int, y :: Int }
  | ConfirmUpdates { events :: Array SvGameEvent }

-- messages client -> server
data ClientMessage
  = ClMoveGid { id :: Int, x :: Int, y :: Int }
  | ClGameStateUpdate { events :: Array ClGameEvent}

-- instances

derive instance genericClGameEvent :: Rep.Generic ClGameEvent _
instance encodeJsonClGameEventt :: EncodeJson ClGameEvent
  where encodeJson = genericEncodeJson
instance decodeJsonClGameEvent :: DecodeJson ClGameEvent
  where decodeJson = genericDecodeJson
instance showGameEventShow :: Show ClGameEvent
  where show = genericShow

derive instance genericSvGameEvent :: Rep.Generic SvGameEvent _
instance encodeJsonSvGameEvent :: EncodeJson SvGameEvent
  where encodeJson = genericEncodeJson
instance decodeJsonSvGameEvent :: DecodeJson SvGameEvent
  where decodeJson = genericDecodeJson
instance svGameEventShow :: Show SvGameEvent
  where show = genericShow

derive instance genericServerMessage :: Rep.Generic ServerMessage _
instance encodeJsonServerMessage :: EncodeJson ServerMessage
  where encodeJson = genericEncodeJson
instance decodeJsonServerMessage :: DecodeJson ServerMessage
  where decodeJson = genericDecodeJson
instance showServerMessage :: Show ServerMessage
  where show = genericShow

derive instance genericClientMessage :: Rep.Generic ClientMessage _
instance encodeJsonClientMessage :: EncodeJson ClientMessage
  where encodeJson = genericEncodeJson
instance decodeClientMessage :: DecodeJson ClientMessage
  where decodeJson = genericDecodeJson
instance showClientMessage :: Show ClientMessage
  where show = genericShow
