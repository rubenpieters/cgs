module Main where

import Prelude

import Data.Maybe
import Data.Tuple
import Data.Array (toUnfoldable, fromFoldable, uncons, cons)
import Data.List hiding (null, length)
import Data.Map as M
import Data.Foldable
import Data.Traversable

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

foreign import phMkCard :: {x :: Int, y :: Int, pack :: Array Card}
                        -> Eff (ph :: PHASER) PhCard
--foreign import cardInfo :: PhCard -> CardInfo
foreign import packInfo :: PhCard -> Eff (ph :: PHASER) Pack
foreign import phaserProps :: PhCard -> Eff (ph :: PHASER) PhaserProps
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
foreign import phLoadTexture :: PhCard -> String -> Int -> Boolean -> Eff (ph :: PHASER) Unit

updateCardSelectMenu :: Eff (ph :: PHASER) Unit
updateCardSelectMenu = do
  gs <- gameState
  sm <- selectMode gs
  case sm of
    (Single c) -> showCardSelectMenu c
    Other -> hideCardSelectMenu

data SelectMode = Single PhCard | Other

selectMode :: GameState -> Eff (ph :: PHASER) SelectMode
selectMode gs = do
  f <- filtered
  pure case f of
    (c:Nil) -> Single c
    _ -> Other
  where
    filtered = filterM (\c -> isSelected c) (M.values gs.cards)

isSelected :: PhCard -> Eff (ph :: PHASER) Boolean
isSelected c = do
  pi <- packInfo c
  pure pi.selected

isDragging :: PhCard -> Eff (ph :: PHASER) Boolean
isDragging c = do
  pi <- packInfo c
  pure pi.dragging

type Pack =
  { pack :: Array Card
  , packText :: String
  , gid :: Int
  , selected :: Boolean
  , dragging :: Boolean
  , overlapped :: Boolean
  }

type Card =
  { texture :: String
  , textureBack :: String
  , faceDir :: FaceDir
  }

newCard :: Card
newCard = { texture: "card", textureBack: "empty", faceDir: FaceUp }

data FaceDir = FaceUp | FaceDown

type PhaserProps =
  { x :: Int
  , y :: Int
  }

type Cid = Int

data GameEvent = Select Cid | Gather | Remove Cid | Flip Cid

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
    update (Flip cid) = onCard cid flipCard

updateCards :: Eff (ph :: PHASER) Unit
updateCards = do
  gs <- gameState
  traverse_ updateCard gs.cards

updateCard :: PhCard -> Eff (ph :: PHASER) Unit
updateCard c = do
  d <- isDragging c
  if d
     then do
            updateDraggedCard c
            mOverlap <- findFirstOverlapCard c
            case mOverlap of
              Just overlap -> do
                updateCardInfo overlap { overlapped: true }
                setTint overlap 0xff0000
              Nothing -> clearOverlaps
     else pure unit

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
  xp <- packInfo c
  gs <- gameState
  let cards = M.values gs.cards
  p1 <- traverse packInfo cards
  let (dropped :: List PhCard) = snd <$> dropWhile (\x -> overlapCondition x (Tuple xp c)) (zip p1 cards)
  pure $ head dropped
  where
    overlapCondition :: Tuple Pack PhCard -> Tuple Pack PhCard -> Boolean
    overlapCondition (Tuple p1 _) (Tuple p2 _) | p1.gid == p2.gid = true
    overlapCondition (Tuple _ c1) (Tuple _ c2) | checkOverlap c1 c2 = false
    overlapCondition _ _ = true

selectCard :: Cid -> Eff (ph :: PHASER) Unit
selectCard cid = do
  onCard cid selectCard'
  --onCard cid toggleSelected
  updateCardSelectMenu

selectCard' :: PhCard -> Eff (ph :: PHASER) Unit
selectCard' c = do
  d <- isDragging c
  s <- isSelected c
  selectAction c {dragging: d, selected: s}

selectAction :: PhCard -> {dragging :: Boolean, selected :: Boolean} -> Eff (ph :: PHASER) Unit
selectAction c p | p.dragging = do
  updateCardInfo c { dragging: false}
selectAction c p | p.selected = do
  updateCardInfo c {selected: false}
  setTint c 0xffffff
selectAction c p | not (p.selected) = do
  updateCardInfo c {selected: true}
  setTint c 0x00ff00
selectAction c p = pure unit


--cardTint :: PhCard -> Color
--cardTint c | isSelected c = rgb 0 255 0
--cardTint c = rgb 0 0 0

{-
toggleSelectProps :: Pack -> {dragging :: Boolean, selected :: Boolean, tint :: String}
toggleSelectProps p | p.dragging = { dragging: false, selected: p.selected, tint: "" }
toggleSelectProps p | p.selected = { selected: false }
toggleSelectProps p | not p.selected = { selected: true }
toggleSelectedProps p = p
-}

flipCard :: PhCard -> Eff (ph :: PHASER) Unit
flipCard c = do
  pi <- packInfo c
  case (uncons pi.pack) of
        Just { head: firstCard, tail: t} -> do
          case firstCard.faceDir of
            FaceUp -> do
              updateCardInfo c { pack: (cons (firstCard {faceDir=FaceDown}) t) }
              phLoadTexture c firstCard.textureBack 0 false
            FaceDown -> do
              updateCardInfo c { pack: (cons (firstCard {faceDir=FaceUp}) t) }
              phLoadTexture c firstCard.texture 0 false
        Nothing -> pure unit

onCard :: Cid
       -> (PhCard -> Eff (ph :: PHASER) Unit)
       -> Eff (ph :: PHASER) Unit
onCard cid f = do
  gs :: GameState <- gameState
  let (mc :: Maybe PhCard) = M.lookup cid gs.cards
  case mc of
    Just (c :: PhCard) -> f c
    Nothing -> pure unit

{-addNewCard :: GameState -> Eff (ph :: PHASER) GameState
addNewCard gs =
  do
    c <- phMkCard { x: 1, y: 1, pack: [{texture: "card"}]}
    pure $ addCard c gs
-}

addCard :: PhCard -> GameState -> Eff (ph :: PHASER) GameState
addCard c gs = do
  pi <- packInfo c
  pure gs { cards = M.insert pi.gid c gs.cards }

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
  selectedCards :: List PhCard <- filterM isSelected (M.values gs.cards)
  if (length selectedCards) <= 1
     then pure unit
     else do
            props <- traverse phaserProps selectedCards
            let avgPos = averagePos props
            packs <- traverse packInfo selectedCards
            let (cards :: List Card) = concat $ (arrayToList <<< _.pack) <$> packs
            c <- phMkCard {x: avgPos.x, y: avgPos.y, pack: listToArray cards}
            -- add all cards to c
            traverse_ phKill selectedCards
            pure unit

arrayToList :: ∀ a. Array a -> List a
arrayToList = toUnfoldable

listToArray :: ∀ a. List a -> Array a
listToArray = fromFoldable

removeCard :: Cid -> Eff (ph :: PHASER) Unit
removeCard cid = onCard cid phKill
