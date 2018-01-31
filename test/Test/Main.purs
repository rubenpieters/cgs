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

import Unsafe.Coerce
import Server
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref
import Test.Expect

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

testRoom = newRef (initialRoomState [[]])
testE1 e map =
  { log: \_ -> pure unit
  , onClMessage: \_ _ -> pure unit
  , onClDisconnect: \_ _ -> pure unit
  , sendMessage: \m c -> do
      log ("sendMessage: " <> (show m) <> " -- to " <> c)
      r <- expectLayerM m "send" e map
      log ("expectResult: " <> show r)
      assert' ("no match: " <> show r) (isMatch r)
  , broadcast: \m { except: c } -> do
      log ("broadcast: " <> (show m) <> " except: " <> c)
      r <- expectLayerM m "bc" e map
      log ("expectResult: " <> show r)
      assert' ("no match: " <> show r) (isMatch r)
  }

expectTest1 =
  Expect [mkE confirmJoinMsg "confirm1_1" "send", mkE newPlayerMsg "bc1" "bc"] $
  Verify ["confirm1_1", "bc1"] verifySameId $
  Expect [mkE confirmJoinMsg "confirm2_2" "send", mkE newPlayerMsg "newPlayer1_2" "send", mkE newPlayerMsg "bc2" "bc"] $
  Verify ["confirm1_1", "confirm2_2"] verifyDiffId $
  Verify ["confirm2_2", "bc2"] verifySameId $
  Done
  where
    confirmJoinMsg (ConfirmJoin _) = true
    confirmJoinMsg _ = false
    newPlayerMsg (NewPlayer _) = true
    newPlayerMsg _ = false
    verifySameId [ConfirmJoin { assignedId: x }, NewPlayer { id: y }] = x == y
    verifySameId _ = false
    verifyDiffId [ConfirmJoin { assignedId: x }, ConfirmJoin { assignedId: y }] = x /= y
    verifyDiffId _ = false


testOnConn :: Eff _ Unit
testOnConn = do
  r <- testRoom
  e <- newRef expectTest1
  map <- newRef M.empty
  onSocketConnection (testE1 e map) r ("NEW_CLIENT_1")
  onSocketConnection (testE1 e map) r ("NEW_CLIENT_2")
  ex <- readRef e
  assert' "expected more messages" (ex # isDone)

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
  let testGs2 = mapCards (dropPack 1) testGs_drop
  let pos0 = (forGid 0 (\(Pack p) -> p.position) testGs2)
  assert (pos0 == Just (OnBoard {x: 0, y: 0}))
  let pos1 = (forGid 1 (\(Pack p) -> p.position) testGs2)
  assert (pos1 == Just (OnBoard {x: 0, y: 0}))
  let pos2 = (forGid 2 (\(Pack p) -> p.position) testGs2)
  assert (pos2 == Just (OnBoard {x: 50, y: 50}))
  log ("testOnConn")
  testOnConn
