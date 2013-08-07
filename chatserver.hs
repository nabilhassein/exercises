{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid            ((<>))
import Data.Text              (Text)
import Control.Exception      (fromException)
import Control.Monad          (forM_, forever)
import Control.Concurrent     (MVar, newMVar, modifyMVar_, readMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

type Client      = (Text, WS.Sink WS.Hybi00)
type ServerState = [Client]

newServerState :: ServerState
newServerState  = []

clientExists :: Client -> ServerState -> Bool
clientExists    (name, _)              = any $ (== name) . fst

addClient :: Client -> ServerState -> ServerState
addClient    client    clients      = if clientExists client clients
                                      then clients
                                      else client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient    (name, _)              = filter $ (/= name) . fst

broadcast :: Text -> ServerState -> IO ()
broadcast    message clients      = do
  T.putStrLn message
  forM_ clients $ \(_, sink) -> WS.sendSink sink $ WS.textData message

main :: IO ()
main = do
  putStrLn "listening for clients..."
  state <- newMVar newServerState
  WS.runServer "127.0.0.1" 9160 $ application state

application :: MVar ServerState -> WS.Request -> WS.WebSockets WS.Hybi00 ()
application    state               req         = do
  WS.acceptRequest req
  WS.getVersion >>= liftIO . putStrLn . ("Client version: " ++)
  WS.sendTextData ("choose a nickname" :: Text)
  msg  <- WS.receiveData
  sink <- WS.getSink
  let client = (msg, sink)
  clients <- liftIO $ readMVar state
  if clientExists client clients
    then WS.sendTextData ("User already exists" :: Text)
    else do liftIO $ modifyMVar_ state $ \s -> do
                let s' = addClient client s
                WS.sendSink sink $ WS.textData $
                  "Welcome! Users: " <> T.intercalate ", " (map fst s')
                broadcast (fst client <> " joined") s'
                return s'
            talk state client

talk :: WS.Protocol p => MVar ServerState -> Client ->        WS.WebSockets p ()
talk                     state               client@(user, _) =
  flip WS.catchWsError catchDisconnect $ forever $ do
    msg <- WS.receiveData
    liftIO $ do
      clients <- readMVar state
      broadcast (user <> ": " <> msg) clients
  where
    catchDisconnect e = case fromException e of
      Just WS.ConnectionClosed -> liftIO $ modifyMVar_ state $ \s -> do
          let s' = removeClient client s
          broadcast (user <> " disconnected") s'
          return s'
      _                        -> return ()
