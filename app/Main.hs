{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Data.String.Strip
import Data.Time.Clock
import Data.Ratio
import Data.Map.Strict
import Control.Concurrent
import Control.Monad.State
import Control.Monad.Reader

data Edge = Edge String String

type EdgeState = UTCTime
type Checker = NominalDiffTime -> Bool

newtype EdgeChecker a = EdgeChecker {
    runEC :: ReaderT Checker (StateT EdgeState IO) a
    } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Checker, MonadState EdgeState)

trigger :: UTCTime -> EdgeChecker Bool
trigger time = do
  checker <- ask
  lastTime <- get
  put time
  return $ checker $ diffUTCTime time lastTime

mkChecker :: Integer -> NominalDiffTime -> Bool
mkChecker diff = (>=) (fromRational (diff % 1000) :: NominalDiffTime)

main :: IO ()
main = do
  t1 <- getCurrentTime
  threadDelay 500000
  t2 <- getCurrentTime
  let diff = diffUTCTime t2 t1
  let checker = mkChecker 500
  print diff
  print $ checker diff

program :: EdgeChecker Bool
program = do
  time <- liftIO getCurrentTime
  trigger time 

mkInitial :: EdgeChecker ()
mkInitial = do
  t <- liftIO getCurrentTime
  put t
  return ()

perse :: EdgeChecker ()
perse = do
  _ <- liftIO getChar
  oldTime <- get
  newTime <- liftIO getCurrentTime
  pass <- trigger newTime
  let diff = diffUTCTime newTime oldTime
  liftIO . putStrLn $ "At " ++ show newTime ++ ": " ++ show pass ++ " delta: " ++ show diff
  perse
    
  
