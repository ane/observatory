{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Concurrent.STM
import qualified Data.Text.Lazy       as T
import           GHC.Generics
import           System.Environment
import qualified Web.Scotty           as S

newtype Config = Config
  { limit :: Int
  } deriving (Eq, Show, Generic)

newtype AppState = AppState
  { counter :: Int
  } deriving (Eq, Show)

data Env = Env
  { envConfig :: Config
  , envState  :: TVar AppState }

newtype MyApp a = MyA
  { runMyApp :: ReaderT Env IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

execApp :: MyApp a -> Int -> Int -> IO a
execApp app d i = do
  init <- newTVarIO (AppState i)
  let env = Env (Config d) init
  runReaderT (runMyApp app) env

getCounter :: MyApp Int
getCounter = do
  env <- ask
  liftIO $ fmap counter (readTVarIO (envState env))

increment :: MyApp ()
increment = do
  env <- ask
  let currState = envState env
  liftIO $ atomically $ modifyTVar' currState (\s -> let c = (counter s) + 1 in AppState c)

prog2 :: MyApp Int
prog2 = replicateM_ 5 increment >> getCounter

main :: IO ()
main = do
  c <- execApp prog2 1 9
  print c
