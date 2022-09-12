{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Application (application)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newMVar)
import Control.Monad (void)
import Lib (newServerState)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Network.WebSockets (runServer)
import Web.Scotty (get, middleware, redirect, scotty)

main :: IO ()
main = do
  void . forkIO $ do
    scotty 3000 $ do
      middleware $ staticPolicy $ addBase "static"
      get "/" $ redirect "/index.html"

  state <- newMVar newServerState
  runServer "127.0.0.1" 8000 $ application state
