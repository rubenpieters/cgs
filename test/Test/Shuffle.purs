module Test.Shuffle where

import Prelude

import Shuffle

import Control.Monad.Eff

import Test.Assert
import Test.Expect

expectShuffle1To10 =
  Expect [ mkE (_ == 1) "lbl" "bkt"
         , mkE (_ == 2) "lbl" "bkt"
         , mkE (_ == 3) "lbl" "bkt"
         , mkE (_ == 4) "lbl" "bkt"
         , mkE (_ == 5) "lbl" "bkt"
         , mkE (_ == 6) "lbl" "bkt"
         , mkE (_ == 7) "lbl" "bkt"
         , mkE (_ == 8) "lbl" "bkt"
         , mkE (_ == 9) "lbl" "bkt"
         , mkE (_ == 10) "lbl" "bkt"
         ] $
  Done

main :: Eff _ Unit
main = do
  let shuffled1 = shuffle "seed" [1,2,3,4,5,6,7,8,9,10]
  let shuffled2 = shuffle "seed" [1,2,3,4,5,6,7,8,9,10]
  assert (shuffled1 == shuffled2)
