module Phaser where

import Prelude

import Data.Foreign.EasyFFI

import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Console

class Subrow (r :: # Type) (s :: # Type)
instance srInst :: Union r t s => Subrow r s

foreign import data PHASER :: Effect

foreign import data PhGame :: Type

type PhGameConfig stateFuncs =
  ( width :: Int
  , height :: Int
  , renderer :: PhRenderer
 -- , parent :: String
  , state :: stateFuncs
  , transparent :: Boolean
  , antialias :: Boolean
 -- , physicsConfig :: 
  )

type PhStateFuncs =
  ( preload :: ∀ e a. Eff (phaser :: PHASER | e) a
  , create :: ∀ e a. Eff (phaser :: PHASER | e) a
  , update :: ∀ e a. Eff (phaser :: PHASER | e) a
  , render :: ∀ e a. Eff (phaser :: PHASER | e) a
  )

data PhRenderer = PhRdrAuto | PhRdrWebGl | PhRdrWebGlMulti | PhRdrCanvas | PhRdrHeadless

-- TODO: create tests for these values
phRendererValue :: PhRenderer -> Int
phRendererValue x = unsafeForeignFunction [] ("Phaser." <> rdrString x)
  where
    rdrString PhRdrAuto = "AUTO"
    rdrString PhRdrWebGl = "WEBGL"
    rdrString PhRdrWebGlMulti = "WEBGLMULTI"
    rdrString PhRdrCanvas = "CANVAS"
    rdrString PhRdrHeadless = "HEADLESS"

-- https://photonstorm.github.io/phaser-ce/Phaser.Game.html
mkGame :: ∀ e r s. Subrow s PhStateFuncs
         => Subrow r (PhGameConfig (Record s))
         => (Record r -> Eff (phaser :: PHASER | e) PhGame)
mkGame = unsafeForeignFunction ["config", ""] "new Phaser.Game(config)"

foreign import createFunc :: ∀ e. PhGame -> Eff (phaser :: PHASER | e) Unit

