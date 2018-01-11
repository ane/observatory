{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where

import           Control.Concurrent.STM
import           Control.Monad.Reader
import qualified Data.Map.Strict                      as M
import qualified Data.Sequence                        as S
import qualified Data.Text.Lazy                       as T
import           Data.Time.Clock
import           GHC.Generics
import           Network.Wai.Middleware.RequestLogger
import           Observatory.Edge
import           Observatory.System
import           Observatory.Types
import           Prelude
import           Web.Scotty.Trans

server :: ScottyT T.Text SystemM ()
server = do
  middleware logStdoutDev
  get "/" $ do
    ev <- liftIO $ newEventNow "aaa"
    let ee = BasicEvent True ev
    doEnqueue <- lift $ enqueue $ NodeEvent "a" "b" ee
    edgeStatus <- lift $ getStatus "a" "b"
    s <- liftIO $ atomically $ edgeStatus <* doEnqueue
    text $ T.pack $ show s
  get "/fail" $ do
    ev <- liftIO $ newEventNow "aaa"
    action <- lift $ enqueue $ NodeEvent "a" "b" (BasicEvent False ev)
    liftIO $ atomically action
    redirect "/"

newEventNow :: T.Text -> IO Event
newEventNow uniqid = fmap (\t -> Event t uniqid) getCurrentTime

newEdge :: IO EdgeState
newEdge = EdgeState BasicEdge (Edge 5 3 2) <$> newTVarIO S.empty

initialize2 :: IO System
initialize2 = do
  edge <- newEdge
  q <- newTQueueIO
  let sys = System (M.fromList [((Node "a", Node "b"), edge)]) q
  _ <- runWorker sys
  return sys

main :: IO ()
main = do
  env <- initialize2
  scottyT 3000 (\a -> runReaderT (runSystemM a) env) server
