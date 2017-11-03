module Main where

import Prelude

import Data.Maybe
import Data.List
import Data.Map

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

type CardInfo =
  { texture :: String
  , pack :: List PhCard
  , packText :: String
  , gid :: Int
  }

type Cid = Int

data UiTrigger = Click Cid

type GameState =
  { cards :: Map Cid PhCard
  }

updateGameState :: UiTrigger -> GameState -> GameState
updateGameState (Click cid) gs = gs { cards = updated }
  where
    updated = update (Just) cid gs.cards
