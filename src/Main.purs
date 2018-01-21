module Main where

import Types

import Server
import WS as WS

import Control.Monad.Eff.Console (log)

main :: Eff _ Unit
main = startServer
       { log: log
       , initializeRooms: initRooms
       , initializeServer: WS.mkServer { port: 8080 }
       , onClConnect: WS.onClConnect
       , onClMessage: WS.onClMessage
       , onClDisconnect: WS.onClDisconnect
       , sendMessage: WS.sendMessage
       , broadcast: WS.broadcast
       }
