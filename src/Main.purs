module Main where

import Prelude

import WS

import Control.Monad.Eff

main :: Eff _ Unit
main = startServer
