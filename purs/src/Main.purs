module Main where

import Prelude

import Data.Maybe
import Data.List hiding (null, length)
import Data.Map as M

import Data.Foldable
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Color

main :: forall e. Eff e Unit
main = do
  pure unit

logTest :: forall e. Eff (console :: CONSOLE | e) Unit
logTest = do
  log "Hello sailor!"

clamp :: Int -> {lBound :: Int, uBound :: Int} -> Int
clamp x b | x > b.uBound = b.uBound
clamp x b | x < b.lBound = b.lBound
clamp x _ = x

{-
data DragMode = Drag | Draw

derive instance eqDragMode :: Eq DragMode
derive instance ordDragMode :: Ord DragMode

nextDragMode :: DragMode -> DragMode
nextDragMode Drag = Draw
nextDragMode Draw = Drag
-}

foreign import data PHASER :: Effect
foreign import data PhCard :: Type

foreign import phMkCard :: {x :: Int, y :: Int, textureName :: String}
                        -> Eff (ph :: PHASER) PhCard
foreign import cardInfo :: PhCard -> CardInfo
foreign import phaserProps :: PhCard -> PhaserProps
foreign import toggleSelected :: PhCard -> Eff (ph :: PHASER) Unit
foreign import showCardSelectMenu :: PhCard -> Eff (ph :: PHASER) Unit
foreign import hideCardSelectMenu :: Eff (ph :: PHASER) Unit
foreign import checkOverlap :: PhCard -> PhCard -> Boolean
foreign import gameState :: Eff (ph :: PHASER) GameState
foreign import updateDraggedCard :: PhCard -> Eff (ph :: PHASER) Unit
foreign import setTint :: PhCard -> Int -> Eff (ph :: PHASER) Unit
--foreign import setCardInfo :: PhCard -> CardInfo -> Eff (ph :: PHASER) Unit
foreign import updateCardInfo :: ∀ e. PhCard -> { | e } -> Eff (ph :: PHASER) Unit

foreign import phKill :: PhCard -> Eff (ph :: PHASER) Unit

updateCardSelectMenu :: Eff (ph :: PHASER) Unit
updateCardSelectMenu = do
  gs <- gameState
  case (selectMode gs) of
    (Single c) -> showCardSelectMenu c
    Other -> hideCardSelectMenu

data SelectMode = Single PhCard | Other

selectMode :: GameState -> SelectMode
selectMode gs = case values of
  (c:Nil) -> Single c
  _ -> Other
  where
    filtered = M.filter (\c -> isSelected c) gs.cards
    values = M.values filtered

isSelected :: PhCard -> Boolean
isSelected c = (cardInfo c).selected

isDragging :: PhCard -> Boolean
isDragging c = (cardInfo c).dragging

type CardInfo =
  { texture :: String
  , pack :: List PhCard
  , packText :: String
  , gid :: Int
  , selected :: Boolean
  , dragging :: Boolean
  , overlapped :: Boolean
  }

type PhaserProps =
  { x :: Int
  , y :: Int
  }

type Cid = Int

data GameEvent = Select Cid | Gather | Remove Cid

type GameState =
  { cards :: M.Map Cid PhCard
  }

emptyGS :: GameState
emptyGS = { cards : M.empty }

updateGameState :: Array GameEvent -> Eff (ph :: PHASER) Unit
updateGameState es = do
  -- handle events
  traverse_ update es
  -- update cards
  updateCards
  where
    update :: GameEvent -> Eff (ph :: PHASER) Unit
    update (Select cid) = selectCard cid
    update Gather = gatherCards
    update (Remove cid) = removeCard cid

updateCards :: Eff (ph :: PHASER) Unit
updateCards = do
  gs <- gameState
  traverse_ updateCard gs.cards

updateCard :: PhCard -> Eff (ph :: PHASER) Unit
updateCard c | isDragging c = do
  updateDraggedCard c
  mOverlap <- findFirstOverlapCard c
  case mOverlap of
    Just overlap -> do
      updateCardInfo overlap { overlapped: true }
      setTint overlap 0xff0000
    Nothing -> clearOverlaps
updateCard _ = pure unit

clearOverlaps :: Eff (ph :: PHASER) Unit
clearOverlaps = do
  gs <- gameState
  traverse_ clearOverlap gs.cards
  where
    clearOverlap :: PhCard -> Eff (ph :: PHASER) Unit
    clearOverlap c = do
      updateCardInfo c { overlapped: false }
      -- TODO: correctly set back to original tint
      setTint c 0xffffff

findFirstOverlapCard :: PhCard -> Eff (ph :: PHASER) (Maybe PhCard)
findFirstOverlapCard c = do
  gs <- gameState
  let (dropped :: List PhCard) = dropWhile (\x -> overlapCondition x c) (M.values gs.cards)
  pure $ head dropped
  where
    overlapCondition :: PhCard -> PhCard -> Boolean
    overlapCondition c1 c2 | (cardInfo c1).gid == (cardInfo c2).gid = true
    overlapCondition c1 c2 | checkOverlap c1 c2 = false
    overlapCondition _ _ = true

selectCard :: Cid -> Eff (ph :: PHASER) Unit
selectCard cid = do
  onCard cid selectCard'
  --onCard cid toggleSelected
  updateCardSelectMenu

selectCard' :: PhCard -> Eff (ph :: PHASER) Unit
selectCard' c | isDragging c = do
  updateCardInfo c { dragging: false}
selectCard' c | isSelected c = do
  updateCardInfo c {selected: false}
  setTint c 0xffffff
selectCard' c | not (isSelected c) = do
  updateCardInfo c {selected: true}
  setTint c 0x00ff00
selectCard' c = pure unit

cardTint :: PhCard -> Color
cardTint c | isSelected c = rgb 0 255 0
cardTint c = rgb 0 0 0

onCard :: Cid
       -> (PhCard -> Eff (ph :: PHASER) Unit)
       -> Eff (ph :: PHASER) Unit
onCard cid f = do
  gs :: GameState <- gameState
  let (mc :: Maybe PhCard) = M.lookup cid gs.cards
  case mc of
    Just (c :: PhCard) -> f c
    Nothing -> pure unit

addNewCard :: GameState -> Eff (ph :: PHASER) GameState
addNewCard gs =
  do
    c <- phMkCard { x: 1, y: 1, textureName: "card" }
    pure $ addCard c gs

addCard :: PhCard -> GameState -> GameState
addCard c gs =  gs { cards = M.insert cid c gs.cards }
    where cid = (cardInfo c).gid

averagePos :: ∀ p f. Foldable f
           => Functor f
           => f {x :: Int, y :: Int | p}
           -> {x :: Int, y :: Int}
averagePos l =
  let {tx:totx, ty:toty, l:length} = foldr (\a {tx:tx,ty:ty,l:l} -> {tx:tx+a.x,ty:ty+a.y,l:l+1}) {tx:0,ty:0,l:0} l in
  {x: totx/length, y: toty/length}

gatherCards :: Eff (ph :: PHASER) Unit
gatherCards = do
  gs <- gameState
  let (selectedCards :: List PhCard) = filter isSelected (M.values gs.cards)
  if (length selectedCards) <= 1
     then pure unit
     else do
            let avgPos = averagePos (phaserProps <$> selectedCards)
            c <- phMkCard {x: avgPos.x, y: avgPos.y, textureName: "card"}
            -- add all cards to c
            traverse_ phKill selectedCards
            pure unit

removeCard :: Cid -> Eff (ph :: PHASER) Unit
removeCard cid = onCard cid phKill
