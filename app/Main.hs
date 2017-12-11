{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Text.Lazy       as T
import           GHC.Generics
import           System.Environment
import qualified Web.Scotty           as S

data Config = Config
  { limit :: Int
  } deriving (Eq, Show, Generic)

data AppState = AppState
  { counter :: Int
  } deriving (Eq, Show)

newtype MyApp a = MyA
  { runMyApp :: ReaderT Config (StateT AppState IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config, MonadState AppState)

execApp :: MyApp a -> Int -> Int -> IO (a, AppState)
execApp app d i =
  let config = Config d
      st = AppState i
  in runStateT (runReaderT (runMyApp app) config) st

getCounter :: MyApp Int
getCounter = fmap counter get

increment :: MyApp ()
increment = do
  s <- get
  let curr = counter s
  put (AppState (curr + 1))

prog2 = replicateM_ 5 increment >> getCounter

main :: IO ()
main = do
  (c, _) <- execApp prog2 1 9
  print c
