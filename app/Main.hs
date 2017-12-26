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

server :: ScottyT T.Text SystemM ()
server = do
  middleware logStdoutDev
  get "/" $ do
    c <- lift $ getStatus "a" "b"
    text $ T.pack $ show c

newEdge :: IO EdgeState
newEdge = do
  let edge = Edge 5 3 2
  state <- newTVarIO S.empty
  return $ EdgeState BasicEdge edge state

initialize2 :: IO System
initialize2 = do
  edge <- newEdge
  return $ System $ M.fromList [((Node "a", Node "b"), edge)]

main :: IO ()
main = do
  env <- initialize2
  scottyT 3000 (\a -> runReaderT (runSystemM a) env) server
