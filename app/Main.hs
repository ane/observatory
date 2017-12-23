{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Control.Monad.Reader
import           Control.Concurrent.STM
import qualified Data.Text.Lazy       as T
import           GHC.Generics
import           System.Environment
import Web.Scotty

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

getCounter :: MyApp Int
getCounter = do
  env <- ask
  liftIO $ fmap counter (readTVarIO (envState env))

increment :: MyApp ()
increment = do
  env <- ask
  let currState = envState env
  liftIO $ atomically $ modifyTVar' currState (\s -> AppState (counter s + 1))

run :: MonadIO m => Env -> MyApp a -> m a
run env prog = liftIO $ runReaderT (runMyApp prog) env

runServer :: Int -> Env -> IO ()
runServer port env = 
  scotty port $ do
    get "/" $ do
      count <- run env $ increment >> getCounter
      html $ T.pack (show count)

initialize :: Int -> Int -> IO Env
initialize m start = do
  state <- newTVarIO (AppState start)
  return $ Env (Config m) state

main :: IO ()
main = do
  env <- initialize 10 0
  runServer 3000 env
