module Test.Main where

import Prelude

import SharedData
import Types
import Pack
import GameState
import WS

import Data.Map as M
import Data.Tuple
import Data.Maybe
import Data.Foldable

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, logShow)

import Test.Assert

testGs :: SharedGameState
testGs = SharedGameState { cardsByGid: testCards }

testCards :: M.Map Gid Pack
testCards = M.fromFoldable ([
  Tuple 0 (Pack { gid: 0
                , cards: cards0
                , position: OnBoard {x: 50, y: 50}
                , lockedBy: Nothing
                })
  , Tuple 1 (Pack { gid: 1
                  , cards: cards1
                  , position: OnBoard {x: 50, y: 50}
                  , lockedBy: Nothing
                  })
  ])
  where
    cards0 = [ mkCardDown "c0" "_" ""
             , mkCardDown "c1" "_" ""
             ]
    cards1 = [ mkCardDown "c2" "_" ""
             , mkCardDown "c3" "_" ""
             ]

testGs_drop :: SharedGameState
testGs_drop = SharedGameState { cardsByGid: testCards_drop }

testCards_drop :: M.Map Gid Pack
testCards_drop = M.fromFoldable ([
  Tuple 0 (Pack { gid: 0
                , cards: cards0
                , position: InHandOf {pid: 1}
                , lockedBy: Nothing
                })
  , Tuple 1 (Pack { gid: 1
                  , cards: cards1
                  , position: OnBoard {x: 50, y: 50}
                  , lockedBy: Just 1
                  })
  , Tuple 2 (Pack { gid: 2
                  , cards: cards2
                  , position: OnBoard {x: 50, y: 50}
                  , lockedBy: Nothing
                  })
  ])
  where
    cards0 = [ mkCardDown "c0" "_" ""
             ]
    cards1 = [ mkCardDown "c1" "_" ""
             ]
    cards2 = [ mkCardDown "c1" "_" ""
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
  let testGs2 = updateCards (dropPack 1) testGs_drop
  let pos0 = (forGid 0 (\(Pack p) -> p.position) testGs2)
  assert (pos0 == Just (OnBoard {x: 0, y: 0}))
  let pos1 = (forGid 1 (\(Pack p) -> p.position) testGs2)
  assert (pos1 == Just (OnBoard {x: 0, y: 0}))
  let pos2 = (forGid 2 (\(Pack p) -> p.position) testGs2)
  assert (pos2 == Just (OnBoard {x: 50, y: 50}))
