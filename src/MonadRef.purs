module MonadRef where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref as R

import Type.Row.Effect.Equality (class EffectRowEquals, effTo)

class MonadRef r m | m -> r where
    newRef :: forall a. a -> m (r a)
    readRef :: forall a. r a -> m a
    writeRef :: forall a. r a -> a -> m Unit
    modifyRef :: forall a. r a -> (a -> a) -> m Unit

instance monadRefRef :: (EffectRowEquals (ref :: R.REF | e) r) => MonadRef R.Ref (Eff r) where
    newRef a = R.newRef a # effTo
    readRef ra = R.readRef ra # effTo
    writeRef ra a  = R.writeRef ra a # effTo
    modifyRef ra f = R.modifyRef ra f # effTo
