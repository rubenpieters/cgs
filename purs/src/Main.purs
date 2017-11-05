module Main where

import Prelude

import Data.Maybe
import Data.List
import Data.Map as M

import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

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

data DragMode = Drag | Draw

derive instance eqDragMode :: Eq DragMode
derive instance ordDragMode :: Ord DragMode

nextDragMode :: DragMode -> DragMode
nextDragMode Drag = Draw
nextDragMode Draw = Drag

foreign import data PHASER :: Effect
foreign import data PhCard :: Type

foreign import phMkCard :: {x :: Int, y :: Int, textureName :: String}
                        -> Eff (ph :: PHASER) PhCard
foreign import cardInfo :: PhCard -> CardInfo
foreign import toggleSelected :: PhCard -> Eff (ph :: PHASER) Unit
foreign import showCardSelectMenu :: PhCard -> Eff (ph :: PHASER) Unit
foreign import hideCardSelectMenu :: Eff (ph :: PHASER) Unit
foreign import checkOverlap :: PhCard -> PhCard -> Boolean
foreign import gameState :: Eff (ph :: PHASER) GameState

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


type CardInfo =
  { texture :: String
  , pack :: List PhCard
  , packText :: String
  , gid :: Int
  , selected :: Boolean
  }

type Cid = Int

data UiEvent = Click Cid

type GameState =
  { cards :: M.Map Cid PhCard
  }

emptyGS :: GameState
emptyGS = { cards : M.empty }

updateGameState :: Array UiEvent -> Eff (ph :: PHASER) Unit
updateGameState es = go (arrayToList es)
  where
    update :: UiEvent -> Eff (ph :: PHASER) Unit
    update (Click cid) = selectCard cid
    -- TODO: not sure if this is sensible, maybe create List on js side?
    arrayToList :: forall a. Array a -> List a
    arrayToList = fromFoldable
    go :: List UiEvent -> Eff (ph :: PHASER) Unit
    go Nil = pure unit
    go (e:es) = update e *> go es


selectCard :: Cid -> Eff (ph :: PHASER) Unit
selectCard cid = do
  gs :: GameState <- gameState
  let (mc :: Maybe PhCard) = M.lookup cid gs.cards
  case mc of
    Just (c :: PhCard) -> toggleSelected c
    Nothing -> pure unit
  updateCardSelectMenu

addNewCard :: GameState -> Eff (ph :: PHASER) GameState
addNewCard gs =
  do
    c <- phMkCard { x: 1, y: 1, textureName: "card" }
    pure $ addCard c gs

addCard :: PhCard -> GameState -> GameState
addCard c gs =  gs { cards = M.insert cid c gs.cards }
    where cid = (cardInfo c).gid

