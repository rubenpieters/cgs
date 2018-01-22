module Shuffle where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Random

foreign import shuffle :: forall a e. String -> Array a -> Array a
