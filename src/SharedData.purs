module SharedData where

import Prelude
import Types
import Pack

import Data.Array
import Data.Either
import Data.Foldable
import Data.Foreign
import Data.Maybe
import Data.Traversable
import Data.Tuple
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
  { playerId :: String
  , displayName :: String
  }

-- GAME STATE

-- gamestate which is shared by every client
-- needs to be obfuscated to hide asymmetric information
data SharedGameState = SharedGameState
  { cardsByGid :: M.Map Gid Pack
  }

emptyGameState :: SharedGameState
emptyGameState = SharedGameState {cardsByGid : M.empty}

forCards :: ∀ t f a.
--            Unfoldable t =>
--            Traversable t =>
            Applicative f =>
            SharedGameState ->
            (Int -> Pack -> f a) ->
            f (Array a)
forCards (SharedGameState state) f = traverse (uncurry f) tupleList
  where
    tupleList = M.toUnfoldable state.cardsByGid

derive instance genericSharedGameState :: Rep.Generic SharedGameState _
instance encodeJsonSharedGameState :: EncodeJson SharedGameState
  where encodeJson = genericEncodeJson
instance decodeJsonSharedGameState :: DecodeJson SharedGameState
  where decodeJson = genericDecodeJson
instance showSharedGameState :: Show SharedGameState
  where show = genericShow

-- TODO: create obfuscation of gamestate

-- obfuscateSharedState :: SharedGameState -> ObfuscatedState

type ObfuscatedGameState = SharedGameState

onGid :: Gid -> (Pack -> Pack) -> SharedGameState -> SharedGameState
onGid gid f (SharedGameState gs) = case M.lookup gid gs.cardsByGid of
    Just (pack :: Pack) -> SharedGameState gs { cardsByGid = M.update (Just <<< f) gid gs.cardsByGid}
    -- TODO: log unexpected gid somewhere?
    Nothing -> SharedGameState gs

forGid :: ∀ a. Gid -> (Pack -> a) -> SharedGameState -> Maybe a
forGid gid f (SharedGameState gs) = f <$> M.lookup gid gs.cardsByGid

setGid :: Gid -> Pack -> SharedGameState -> SharedGameState
setGid gid pack (SharedGameState gs) =
  SharedGameState (gs { cardsByGid = M.insert gid pack gs.cardsByGid })

removeGid :: Gid -> SharedGameState -> SharedGameState
removeGid gid (SharedGameState gs) =
  SharedGameState (gs { cardsByGid = M.delete gid gs.cardsByGid })

flipTop :: Pack -> Pack
flipTop (Pack p) = Pack $ case head p.cards of
  Just topCard -> p { cards = cons (flipCard topCard) otherCards }
  Nothing -> p
  where
    otherCards = case tail p.cards of
      Just o -> o
      Nothing -> []

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

derive instance genericClGameEvent :: Rep.Generic ClGameEvent _
instance encodeJsonClGameEventt :: EncodeJson ClGameEvent
  where encodeJson = genericEncodeJson
instance decodeJsonClGameEvent :: DecodeJson ClGameEvent
  where decodeJson = genericDecodeJson
instance showGameEventShow :: Show ClGameEvent
  where show = genericShow

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

derive instance genericSvGameEvent :: Rep.Generic SvGameEvent _
instance encodeJsonSvGameEvent :: EncodeJson SvGameEvent
  where encodeJson = genericEncodeJson
instance decodeJsonSvGameEvent :: DecodeJson SvGameEvent
  where decodeJson = genericDecodeJson
instance svGameEventShow :: Show SvGameEvent
  where show = genericShow

-- messages server -> client
data ServerMessage
  = ConfirmJoin { assignedId :: Int, roomGameState :: ObfuscatedGameState }
  | NewPlayer { id :: Int }
  | SvMoveGid { id :: Int, x :: Int, y :: Int }
  | ConfirmUpdates { events :: Array SvGameEvent }

derive instance genericServerMessage :: Rep.Generic ServerMessage _
instance encodeJsonServerMessage :: EncodeJson ServerMessage
  where encodeJson = genericEncodeJson
instance decodeJsonServerMessage :: DecodeJson ServerMessage
  where decodeJson = genericDecodeJson
instance showServerMessage :: Show ServerMessage
  where show = genericShow

-- messages client -> server
data ClientMessage
  = ClMoveGid { id :: Int, x :: Int, y :: Int }
  | ClGameStateUpdate { events :: Array ClGameEvent}

derive instance genericClientMessage :: Rep.Generic ClientMessage _
instance encodeJsonClientMessage :: EncodeJson ClientMessage
  where encodeJson = genericEncodeJson
instance decodeClientMessage :: DecodeJson ClientMessage
  where decodeJson = genericDecodeJson
instance showClientMessage :: Show ClientMessage
  where show = genericShow
