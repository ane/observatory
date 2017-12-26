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
  liftIO $ atomically $
    modifyTVar' (envState env) (\s -> AppState (counter s + 1))

server :: ScottyT T.Text MyApp ()
server = do
  middleware logStdoutDev
  get "/" $ do
    c <- lift getCounter
    _ <- lift increment
    text $ T.pack $ show c

initialize :: Int -> Int -> IO Env
initialize m start = do
  state <- newTVarIO (AppState start)
  return $ Env (Config m) state

main :: IO ()
main = do
  env <- initialize 10 0
  scottyT 3000 (\a -> runReaderT (runMyApp a) env) server
