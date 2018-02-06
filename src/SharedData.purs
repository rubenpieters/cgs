module SharedData where

import Control.Monad.Except
import Data.Array
import Data.Foldable
import Data.Foreign
import Data.Traversable
import Data.Unfoldable
import GameState
import Pack
import Shuffle
import Types

import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrowException)
import Control.Monad.Reader (ask)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep as Rep
import Data.Generic.Rep.Show (genericShow)
import Data.Map as M

type Player =
  { id :: Int
  }

type ObfuscatedGameState = SharedGameState

updateSharedGameState :: SvGameEvent -> SharedGameState -> SharedGameState
updateSharedGameState (SvFlip gid) gs = onGid gid (\(Pack p) -> Pack (flipTop p)) gs
updateSharedGameState (SvLock gid { pid: pid }) gs = onGid gid (\(Pack p) -> Pack (lockPack pid p)) gs
updateSharedGameState (SvLockDeny) gs = gs
updateSharedGameState (SvDraw gid { amount: amount, newGid: newGid }) gs =
  case (forGid gid (\(Pack p) -> p # drawFromPack amount) gs) of
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
updateSharedGameState (SvShuffle gid { seed: seed }) gs =
  case (forGid gid id gs) of
    Just (Pack pack) ->
      let shuffled = shuffle seed pack.cards
      in setGid gid (Pack (pack {cards= shuffled})) gs
    Nothing -> gs
updateSharedGameState (SvActionDeny _) gs = gs

-- CLIENT GAME EVENT
-- client sends to server

data ClGameEvent = ClFlip Gid
                 | ClLock Gid
                 | ClDraw Gid { amount :: Int }
                 | ClDrop Gid { x :: Int, y :: Int }
                 | ClDropIn Gid { tgt :: Gid }
                 | ClToHand Gid
                 | ClShuffle Gid

-- SERVER GAME EVENT
-- server sends to client
-- server adds information where necessary

data SvGameEvent = SvFlip Gid
                 | SvLock Gid { pid :: Int }
                 | SvLockDeny
                 | SvDraw Gid { amount :: Int, newGid :: Int }
                 | SvDrop Gid { x :: Int, y :: Int }
                 | SvDropIn Gid { tgt :: Gid }
                 | SvToHand Gid { pid :: Int }
                 | SvShuffle Gid { seed :: String }
                 | SvActionDeny Gid

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

-- TODO: move definitions to correct module


-- WIP creating generalization of server and client update gamestate functions
genericUpdate :: forall pack f a r.
                 (Monad f) =>
                 { log :: String -> f Unit
                 , throw :: forall a. String -> f a
                 -- TODO: pack or (Maybe pack) ?
                 , packByGid :: Gid -> f (Maybe pack)
                 , getPackData :: pack -> f (PackData r)
                 , setPackData :: (PackData r) -> pack -> f Unit
                 , createPack :: forall x. (PackData x) -> f Unit
                 , deletePack :: pack -> f Unit
                 , packToHand :: PlayerId -> pack -> f Unit
                 , dropAt :: { x :: Int, y :: Int } -> pack -> f Unit
                 } ->
                 PlayerId ->
                 SvGameEvent ->
                 f Unit
genericUpdate k _ (SvFlip gid) = do
  pack <- packByGidOrThrow k gid
  packData <- pack # k.getPackData
  pack # k.setPackData (f packData)
  where
    f = flipTop
genericUpdate k _ (SvLock gid { pid: pid }) = do
  pack <- packByGidOrThrow k gid
  packData <- pack # k.getPackData
  pack # k.setPackData (f packData)
  where
    f = lockPack pid
genericUpdate k _ (SvLockDeny) = do
  pure unit -- TODO: should denying locks/actions be separate?
genericUpdate k _ (SvActionDeny _) = do
  pure unit
genericUpdate k pid (SvDraw gid { amount: amount, newGid: newGid }) = do
  pack <- packByGidOrThrow k gid
  packData <- pack # k.getPackData
  let { remaining: remaining, drawn: drawn } = packData # drawFromPack amount
  -- set cards of old pack to remaining cards
  pack # k.setPackData (packData { cards= remaining })
  -- create new pack from drawn cards
  let newPack = { gid: newGid
                , cards: drawn
                , lockedBy: Just pid
                }
  k.createPack newPack
genericUpdate k _ (SvDrop gid pos) = do
  pack <- packByGidOrThrow k gid
  pack # k.dropAt pos
genericUpdate k _ (SvDropIn drp { tgt: pk }) = do
  pk <- packByGidOrThrow k pk
  pkData <- pk # k.getPackData
  drp <- packByGidOrThrow k drp
  drpData <- drp # k.getPackData
  pk # k.setPackData (pkData { cards= pkData.cards <> drpData.cards })
  drp # k.deletePack
genericUpdate k _ (SvToHand gid { pid: pid }) = do
  pack <- packByGidOrThrow k gid
  pack # k.packToHand pid
genericUpdate k _ (SvShuffle gid { seed: seed }) = do
  pack <- packByGidOrThrow k gid
  packData <- pack # k.getPackData
  pack # k.setPackData (f packData)
  where
    f r = r { cards= shuffle seed r.cards }

packByGidOrThrow :: forall pack f a r.
                 (Monad f) =>
                 { log :: String -> f Unit
                 , throw :: forall a. String -> f a
                 , packByGid :: Gid -> f (Maybe pack)
                 | r } ->
                 Gid -> f pack
packByGidOrThrow k gid = do
  mPack <- k.packByGid gid
  case mPack of
    Just pack -> pure pack
    Nothing -> k.throw ("Pack " <> show gid <> "not found!")
