{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where

import           Control.Concurrent.STM
import           Control.Monad.Reader
import qualified Data.Text.Lazy                       as T
import           GHC.Generics
import           Network.Wai.Middleware.RequestLogger
import           Prelude
import           Web.Scotty.Trans
import           Observatory.System
import           Observatory.Types
import           Observatory.Edge
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import           Data.Time.Clock

server :: ScottyT T.Text SystemM ()
server = do
  middleware logStdoutDev
  get "/" $ do
    ev <- liftIO $ newEventNow "aaa"
    let ee = BasicEvent True ev
    lift $ enqueue $ NodeEvent "a" "b" ee
    c <- lift $ getStatus "a" "b"
    s <- liftIO $ atomically c
    text $ T.pack $ show s

newEventNow id = do
  now <- getCurrentTime
  return $ Event now id

newEdge :: IO EdgeState
newEdge = do
  let edge = Edge 5 3 2
  state <- newTVarIO S.empty
  return $ EdgeState BasicEdge edge state

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
