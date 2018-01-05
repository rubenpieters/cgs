module Test.Main where

import Prelude

import SharedData

import Data.Map as M
import Data.Tuple
import Data.Maybe
import Data.Foldable

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, logShow)

import Test.Assert

testGs :: SharedGameState
testGs = SharedGameState { cardsByGid : testCards }

testCards :: M.Map Gid Pack
testCards = M.fromFoldable ([
  Tuple 0 (Pack { gid : 0
                , cards : cards0
                , position : Pos {x : 50, y : 50}
                , lockedBy : Nothing
                , inHandOf : Nothing
                })
  , Tuple 1 (Pack { gid : 1
                  , cards : cards1
                  , position : Pos {x : 50, y : 50}
                  , lockedBy : Nothing
                  , inHandOf : Nothing
                  })
  ])
  where
    cards0 = [ mkCardDown "c0" "_"
             , mkCardDown "c1" "_"
             ]
    cards1 = [ mkCardDown "c2" "_"
             , mkCardDown "c3" "_"
             ]

main :: Eff _ Unit
main = do
  let testGs1 = updateSharedGameState (SvDropIn 1 { tgt: 0 }) testGs
  case (forGid 0 (\(Pack p) -> p.cards) testGs1) of
    Just (cards :: Array Card) -> do
      let (cardAmt :: Int) = length cards
      log ("cards: " <> show cardAmt)
      logShow cards
      assert (cardAmt == 4)
    Nothing -> assert' "no gid 0" false
