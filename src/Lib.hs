module Lib
  ( Client,
    ServerState,
    newServerState,
    numClients,
    clientExists,
    addClient,
    removeClient,
    broadcast,
  )
where

import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Network.WebSockets as WS

type Client = (Text, WS.Connection)

type ServerState = [Client]

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

addClient :: Client -> ServerState -> ServerState
addClient client state
  | clientExists client state = state
  | otherwise = client : state

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
  TIO.putStrLn message
  forM_ clients $ \(_, conn) -> WS.sendTextData conn message
