{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Application (application) where

import Control.Concurrent.MVar (MVar, modifyMVar_, readMVar)
import Control.Monad (forever)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), decode, encode, pairs, (.:), (.=))
import Data.Aeson.Text (encodeToLazyText)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import GHC.Generics (Generic)
import Lib (Client, ServerState, addClient, broadcast, clientExists)
import qualified Network.WebSockets as WS
import System.Random (randomRIO)
import Templates (makeTask)
import Text.Blaze.Html.Renderer.Text (renderHtml)

data Event = Event {type_ :: !Text, value :: !Text} deriving (Show, Generic)

instance ToJSON Event where
  toEncoding (Event t v) = pairs ("type" .= t <> "value" .= v)

instance FromJSON Event where
  parseJSON (Object v) = Event <$> v .: "type" <*> v .: "value"
  parseJSON _ = fail "Expected an object for Event"

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ do
    msg <- WS.receiveData conn
    let prefix = "Hi! I am "
        client = (T.drop (T.length prefix) msg, conn)

    clients <- readMVar state
    if clientExists client clients
      then sendEncodeEvent client $ Event "warning" "Client already exists"
      else do
        modifyMVar_ state $ \s -> do
          let s' = addClient client s
          sendEncodeEvent client $
            Event "connected" ("Client added " <> T.intercalate ", " (map fst s'))
          broadcast (toStrict . encodeToLazyText $ Event "connected" (fst client <> " joined")) s'
          return s'
    talk client state

talk :: Client -> MVar ServerState -> IO ()
talk client@(_, conn) _ = forever $ do
  msg <- WS.receiveData conn
  let event = decode msg :: Maybe Event

  case event of
    Nothing -> sendEncodeEvent client $ Event "warning" "Invalid message"
    Just event' -> case type_ event' of
      "add-task" -> addTask client event'
      "clear-all" -> sendEncodeEvent client $ Event "clear-all" "Cleaning all tasks"
      "random" -> do
        n <- randomRIO (1, 100) :: IO Int
        sendEncodeEvent client $ Event "random" (T.pack $ show n)
      _ -> sendEncodeEvent client $ Event "warning" "Invalid message"

addTask :: Client -> Event -> IO ()
addTask client event =
  let task = value event
   in sendEncodeEvent client $ Event "task-added" $ toStrict . renderHtml $ makeTask task

sendEncodeEvent :: Client -> Event -> IO ()
sendEncodeEvent (_, conn) = WS.sendTextData conn . encode
