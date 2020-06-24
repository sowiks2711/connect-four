module Main where

import Network.Wai.Handler.Warp
import Servant
import Server


mainServer :: IO ()
mainServer = do
  initialState <- initializeState
  run 8081 $ app initialState

main :: IO ()
main = mainServer
