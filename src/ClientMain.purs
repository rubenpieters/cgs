module ClientMain where

import SharedData

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Maybe
import Data.Either
import Data.Tuple
import Data.Array (toUnfoldable, fromFoldable, uncons, cons, take, drop)
import Data.List hiding (null, length)
import Data.Map as M
import Data.Foldable
import Data.Traversable

import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Exception.Unsafe (unsafeThrowException)
import Control.Monad.Except.Trans (runExceptT)

import Data.Either
import Data.List.NonEmpty (NonEmptyList)
import Data.Newtype (unwrap)
import Data.Foreign
import Data.Foreign.Class (class Encode, class Decode)
import Data.Foreign.Generic as DFG
import Data.Foreign.Generic (genericEncode, encodeJSON, genericDecode, decodeJSON)
import Data.Generic.Rep as Rep
import Data.Generic.Rep.Show (genericShow)


main :: forall e. Eff e Unit
main = pure unit

clamp :: Int -> {lBound :: Int, uBound :: Int} -> Int
clamp x b | x > b.uBound = b.uBound
clamp x b | x < b.lBound = b.lBound
clamp x _ = x

foreign import data PHASER :: Effect
foreign import data PhCard :: Type

foreign import phMkCard :: ∀ e. {x :: Int, y :: Int, pack :: Array Card}
                        -> Eff (ph :: PHASER | e) PhCard
foreign import phaserProps :: ∀ e. ClPack -> Eff (ph :: PHASER | e) PhaserProps
foreign import toggleSelected :: ∀ e. PhCard -> Eff (ph :: PHASER | e) Unit
foreign import showCardSelectMenu :: ∀ e. PhCard -> Eff (ph :: PHASER | e) Unit
foreign import hideCardSelectMenu :: ∀ e. Eff (ph :: PHASER | e) Unit
foreign import checkOverlap :: ClPack -> ClPack -> Boolean
foreign import gameState :: ∀ e. Eff (ph :: PHASER | e) GameState
foreign import updateDraggedCard :: ∀ e. ClPack -> Eff (ph :: PHASER | e) Unit
foreign import isConnected :: ∀ e. Eff (ph :: PHASER | e) Boolean
foreign import moveCard :: ∀ e. Int -> Int -> ClPack -> Eff (ph :: PHASER | e) Unit
foreign import setTint :: ∀ e. Int -> ClPack -> Eff (ph :: PHASER | e) Unit
foreign import updateCardInfo :: ∀ e r. PhCard -> { | r } -> Eff (ph :: PHASER | e) Unit
foreign import updatePackText :: ∀ e. ClPack -> Eff (ph :: PHASER | e) Unit

foreign import phKill :: ∀ e. ClPack -> Eff (ph :: PHASER | e) Unit
foreign import phLoadTexture :: ∀ e. ClPack -> String -> Int -> Boolean -> Eff (ph :: PHASER | e) Unit

data SelectMode = Single ClPack | Other

selectMode :: ∀ e. LocalGameState -> Eff (ph :: PHASER | e) SelectMode
selectMode (LocalGameState gs) = do
  f <- filtered
  pure case f of
    (c:Nil) -> Single c
    _ -> Other
  where
    filtered = filterM (\c -> isSelected c) (M.values gs.cardsByGid)

isSelected :: ∀ e. ClPack -> Eff (ph :: PHASER | e) Boolean
isSelected c = do
  props <- c # getProps
  pure props.selected

isDragging :: ∀ e. ClPack -> Eff (ph :: PHASER | e) Boolean
isDragging c = do
  props <- c # getProps
  pure props.dragging

type PhaserProps =
  { x :: Int
  , y :: Int
  }

type Cid = Int

showGameEvent :: ClGameEvent -> String
showGameEvent = show

type GameState =
  { cards :: M.Map Cid PhCard
  }

emptyGS :: LocalGameState
emptyGS = LocalGameState { cardsByGid : M.empty }

data LockStatus = LockedBySelf | LockedByOther | NotLocked

lockStatus :: ClPack -> Eff _ LockStatus
lockStatus c = do
  props <- c # getProps
  -- TODO: get player id and check with that instead of `1`
  let res = case (props.lockedBy) of
              Just playerId -> if playerId == 1
                then LockedBySelf
                else LockedByOther
              Nothing -> NotLocked
  pure res

lockStatusGid :: Gid -> Eff _ LockStatus
lockStatusGid gid = onCard' gid lockStatus

cardLocked :: Gid -> Eff _ Boolean
cardLocked gid = do
  lockStatus <- lockStatusGid gid
  case lockStatus of
    LockedBySelf -> pure true
    LockedByOther -> pure true
    NotLocked -> pure false

updateGameState :: Array SvGameEvent -> Eff _ Unit
updateGameState es = do
  -- handle events
  traverse_ update es
  -- update cards
--  updateCards
  where
    update :: SvGameEvent -> Eff _ Unit
    --update (Select cid) = selectCard cid
    --update Gather = gatherCards
    --update (Remove cid) = removeCard cid
    update (SvSelect gid) = unsafeThrowException (error "unimplemented")
    update SvGather = unsafeThrowException (error "unimplemented")
    update (SvRemove gid) = unsafeThrowException (error "unimplemented")
    update (SvFlip gid) = onCard gid flipCard
    update (SvLock gid) = onCard gid lockCard
    update (SvDraw gid p) = onCard gid (drawX p)
    update (SvDrop gid _) = onCard gid dropCard
    update (SvDropIn drp { tgt: pk }) = onCard2 pk drp dropInCard

updateCards :: Eff _ Unit
updateCards = do
  (LocalGameState gs) <- getGameState
  traverse_ updateCard gs.cardsByGid

updateCard :: ClPack -> Eff _ Unit
updateCard c = do
  props <- c # getProps
  --log ("updating card " <> show props.gid)
  if props.dragging
     then do
            clearOverlaps
            updateDraggedCard c
            connected <- isConnected
            if connected
               then do
                    socket <- getSocket
                    phProps <- c # phaserProps
                    socket `emit` (ClMoveGid {id: props.gid, x: phProps.x, y: phProps.y})
               else pure unit
            mOverlap <- findFirstOverlapCard c
            case mOverlap of
              Just overlap -> do
                props <- overlap # getProps
                overlap # setProps (props { overlapped= true })
                overlap # setTint 0x55ff55
                setOverlapCard overlap
              Nothing -> pure unit
     else pure unit

clearOverlaps :: Eff _ Unit
clearOverlaps = do
  (LocalGameState gs) <- getGameState
  clearOverlapCard
  traverse_ clearOverlap gs.cardsByGid
  where
    clearOverlap :: ClPack -> Eff _ Unit
    clearOverlap c = do
      props <- c # getProps
      let newProps = props { overlapped = false }
      c # setProps newProps
      c # setColor

findFirstOverlapCard :: ClPack -> Eff _ (Maybe ClPack)
findFirstOverlapCard c = do
  props <- c # getProps
  (LocalGameState gs) <- getGameState
  let cards = M.values gs.cardsByGid
  p1 <- traverse getProps cards
  let (dropped :: List ClPack) = snd <$> dropWhile (\x -> overlapCondition x (Tuple props c)) (zip p1 cards)
  pure $ head dropped
  where
    overlapCondition :: forall r. Tuple {gid :: Gid | r} ClPack -> Tuple {gid :: Gid | r} ClPack -> Boolean
    overlapCondition (Tuple p1 _) (Tuple p2 _) | p1.gid == p2.gid = true
    overlapCondition (Tuple _ c1) (Tuple _ c2) | checkOverlap c1 c2 = false
    overlapCondition _ _ = true

setColor :: ClPack -> Eff _ Unit
setColor c = do
  props <- c # getProps
  c # setTint (packTint props)

packTint :: PackProps -> Int
packTint p | p.dragging = 0xff0000
packTint p = 0xffffff

flipCard :: ClPack -> Eff _ Unit
flipCard c = do
  props <- c # getProps
  case (uncons props.cards) of
        Just { head: (Card firstCard), tail: t} -> do
          case firstCard.faceDir of
            FaceUp -> do
              let (newCards :: Array Card) = (cons (Card $ firstCard {faceDir=FaceDown}) t)
              let (newProps :: PackProps) = (props { cards = newCards})
              c # setProps newProps
              phLoadTexture c firstCard.textureBack 0 false
            FaceDown -> do
              let (newCards :: Array Card) = (cons (Card $ firstCard {faceDir=FaceUp}) t)
              let (newProps :: PackProps) = (props { cards = newCards})
              c # setProps newProps
              phLoadTexture c firstCard.textureFront 0 false
        -- if pack is empty, flip does nothing
        Nothing -> pure unit

lockCard :: ClPack -> Eff _ Unit
lockCard c = do
  props <- c # getProps
  let (newProps :: PackProps) = (props { lockedBy = Just 1 })
  c # setProps newProps
  activateDragTrigger
  c # setTint 0xff0000

dropCard :: ClPack -> Eff _ Unit
dropCard c = do
  props <- c # getProps
  log ("dropping card" <> show props.gid)
  let (newProps :: PackProps) = (props { lockedBy = Nothing, dragging = false })
  c # setProps newProps
  c # setTint 0xffffff

dropInCard :: ClPack -> ClPack -> Eff _ Unit
dropInCard pk drp = do
  pkProps <- pk # getProps
  drpProps <- drp # getProps
  let (newPkProps :: PackProps) = (pkProps { cards = pkProps.cards <> drpProps.cards })
  phKill drp
  pk # setProps newPkProps
  pk # updatePackText
  -- do something extra if src had 0 cards?
  pure unit

drawX :: { amount :: Int, newGid :: Int } -> ClPack -> Eff _ Unit
drawX { amount : x, newGid : newGid } c = do
  props <- c # getProps
  log ("drawing " <> show x <> " from " <> show props.gid)
  let drawnCards = take x props.cards
  let leftoverCards = drop x props.cards
  c # setProps (props { cards= leftoverCards })
  c # updatePackText
  let newPackInfo = { gid: newGid, cards: drawnCards, lockedBy: Just 1}
  newC <- materializeCard {x : 30, y : 30, texture : "empty", size : (drawnCards # length), pack : newPackInfo}
  newProps <- newC # getProps
  newC # setProps (newProps { dragging= true })
  setDragTrigger newC
  newC # setTint 0xff0000
  (LocalGameState gs) <- getGameState
  let updatedGs = LocalGameState ({ cardsByGid : gs.cardsByGid # M.insert newGid newC})
  setGameState updatedGs

onCard :: ∀ e. Gid
       -> (ClPack -> Eff (ph :: PHASER | e) Unit)
       -> Eff (ph :: PHASER | e) Unit
onCard gid f = do
  (LocalGameState gs) <- getGameState
  let (mc :: Maybe ClPack) = M.lookup gid gs.cardsByGid
  case mc of
    Just (c :: ClPack) -> f c
    Nothing -> pure unit

onCard2 :: ∀ e. Gid -> Gid
       -> (ClPack -> ClPack -> Eff (ph :: PHASER | e) Unit)
       -> Eff (ph :: PHASER | e) Unit
onCard2 gid1 gid2 f = do
  (LocalGameState gs) <- getGameState
  let (mc1 :: Maybe ClPack) = M.lookup gid1 gs.cardsByGid
  let (mc2 :: Maybe ClPack) = M.lookup gid2 gs.cardsByGid
  case mc1 of
    Just (c1 :: ClPack) -> case mc2 of
      Just (c2 :: ClPack) -> f c1 c2
      Nothing -> pure unit
    Nothing -> pure unit

onCard' :: ∀ a e. Gid
       -> (ClPack -> Eff (ph :: PHASER | e) a)
       -> Eff (ph :: PHASER | e) a
onCard' gid f = do
  (LocalGameState gs) <- getGameState
  let (mc :: Maybe ClPack) = M.lookup gid gs.cardsByGid
  case mc of
    Just (c :: ClPack) -> f c
    Nothing -> unsafeThrowException (error ("card " <> show gid <> " does not exist"))

-- TODO: keep track of killed cards, so they can be revived
removeCardGS :: ∀ e. ClPack -> LocalGameState -> Eff (ph :: PHASER | e) LocalGameState
removeCardGS c (LocalGameState gs) = do
  props <- c # getProps
  pure $ LocalGameState $ gs { cardsByGid = M.delete props.gid gs.cardsByGid }

averagePos :: ∀ p f. Foldable f
           => Functor f
           => f {x :: Int, y :: Int | p}
           -> {x :: Int, y :: Int}
averagePos l =
  let {tx:totx, ty:toty, l:length} = foldr (\a {tx:tx,ty:ty,l:l} -> {tx:tx+a.x,ty:ty+a.y,l:l+1}) {tx:0,ty:0,l:0} l in
  {x: totx/length, y: toty/length}

gatherCards :: ∀ e. Eff (ph :: PHASER | e) Unit
gatherCards = do
  (LocalGameState gs) <- getGameState
  selectedCards :: List ClPack <- filterM isSelected (M.values gs.cardsByGid)
  if (length selectedCards) <= 1
     then pure unit
     else do
            props <- traverse phaserProps selectedCards
            let avgPos = averagePos props
            packs <- traverse getProps selectedCards
            let (cards :: List Card) = concat $ (arrayToList <<< _.cards) <$> packs
            c <- phMkCard {x: avgPos.x, y: avgPos.y, pack: listToArray cards}
            -- add all cards to c
            traverse_ phKill selectedCards
            pure unit

arrayToList :: ∀ a. Array a -> List a
arrayToList = toUnfoldable

listToArray :: ∀ a. List a -> Array a
listToArray = fromFoldable

-- server interacting code, move to new module (client/server) ?

--foreign import data NETWORK :: Effect
foreign import data Socket :: Type

foreign import getSocket :: ∀ e. Eff (ph :: PHASER | e) Socket
foreign import unsafeEmit :: ∀ e. Socket -> String -> Eff (ph :: PHASER | e) Unit

emit :: ∀ e msg. (EncodeJson msg) => Socket -> msg -> Eff (ph :: PHASER | e) Unit
emit socket msgStr = unsafeEmit socket (stringify $ encodeJson msgStr)

sendUpdates :: ∀ e. Socket -> Array ClGameEvent-> Eff (ph :: PHASER | e) Unit
sendUpdates socket events = emit socket (ClGameStateUpdate { events: events })

-- client on server message

onServerStrMessage :: ∀ e.
                      String -> Eff (console :: CONSOLE, ph :: PHASER | e) Unit
onServerStrMessage msg = do
  let eSvMsg = jsonParser msg >>= decodeJson
  case eSvMsg of
    Left errs -> log "malformed message"
    Right (svMsg :: ServerMessage) -> onServerMessage svMsg

onServerMessage :: ∀ e.
                   ServerMessage -> Eff (console :: CONSOLE, ph :: PHASER | e) Unit
onServerMessage (ConfirmJoin {assignedId: id, roomGameState: gs}) = do
  log ("assigned player id: " <> show id)
  clearPhaserState
  lgs <- materializeState gs
  setGameState lgs
onServerMessage (NewPlayer {id: id}) = do
  log ("new player connected: " <> show id)
onServerMessage (SvMoveGid {id: id, x: x, y: y}) = do
  log ("mov gid, id: " <> show id <> ", x: " <> show x <> ", y: " <> show y)
  onCard id (moveCard x y)
onServerMessage (ConfirmUpdates {events: events}) = do
  log ("confirmed updates: " <> show events)
  updateGameState events

foreign import data ClPack :: Type

type PackInfoX r =
  { gid :: Gid
  , cards :: Array Card
  , lockedBy :: Maybe PlayerId
  | r
  }

foreign import clearPhaserState :: ∀ e. Eff (ph :: PHASER | e) Unit

foreign import materializeCard :: ∀ e r. { x :: Int, y :: Int, texture :: String, size :: Int, pack :: PackInfoX r } -> Eff (ph :: PHASER | e) ClPack

foreign import getGameState :: ∀ e. Eff (ph :: PHASER | e) LocalGameState
foreign import setGameState :: ∀ e. LocalGameState -> Eff (ph :: PHASER | e) Unit

data LocalGameState = LocalGameState
  { cardsByGid :: M.Map Gid ClPack
  }

materializeCard' :: Pack -> Eff _ ClPack
materializeCard' (Pack pack) = do
  let create texture = materializeCard { texture : texture, x : pack.position # \(Pos p) -> p.x, y : pack.position # \(Pos p) -> p.y, size : length pack.cards, pack : pack }
  case uncons pack.cards of
    Just { head : card, tail : _} -> do
      create (card # cardTexture)
    Nothing -> do
      create "empty"

materializeState :: ObfuscatedGameState -> Eff _ LocalGameState
materializeState (SharedGameState gs) = do
  lgs <- traverse f gs.cardsByGid
  pure (LocalGameState {cardsByGid : lgs})
  where
    f pack = do
               clPack <- materializeCard' pack
--               pure { pack : pack, clPack : clPack }
               pure clPack

type PackProps =
  { selected :: Boolean
  , dragging :: Boolean
  , overlapped :: Boolean
  | PackInfo
  }

foreign import getProps :: ∀ e. ClPack -> Eff (ph :: PHASER | e) PackProps
foreign import setProps :: ∀ e. PackProps -> ClPack -> Eff (ph :: PHASER | e) Unit

foreign import getOverlapCard :: ∀ e. Eff (ph :: PHASER | e) ClPack
foreign import clearOverlapCard :: ∀ e. Eff (ph :: PHASER | e) Unit
foreign import setOverlapCard :: ∀ e. ClPack -> Eff (ph :: PHASER | e) Unit

foreign import activateDragTrigger :: ∀ e. Eff (ph :: PHASER | e) Unit
foreign import setDragTrigger :: ∀ e. ClPack -> Eff (ph :: PHASER | e) Unit
