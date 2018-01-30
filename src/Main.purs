module Main where

import Types

import Server
import WS as WS

import Control.Monad.Eff.Console (log)

main :: Eff _ Unit
main = do
  log "Initializing Rooms"
  roomStates <- initRooms
  log "Starting Server"
  server <- WS.mkServer { port: 8080 }
  let k =
       { log: log
       , onClMessage: WS.onClMessage
       , onClDisconnect: WS.onClDisconnect
       , sendMessage: WS.sendMessage
       , broadcast: \a b -> WS.broadcast a b server
       }
  server # WS.onClConnect (onSocketConnection k roomStates)

