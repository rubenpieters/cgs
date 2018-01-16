module Main where

import Types

import Server

main :: Eff _ Unit
main = startServer
