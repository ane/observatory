{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.String.Strip
import Data.Time.Clock
import Data.Ratio
import Data.Map.Strict as M
import Control.Concurrent
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Text as T

data Edge a = Edge String String (EdgeChecker a)

type EdgeMap a = M.Map (T.Text, T.Text) (Edge a)

type EdgeState = UTCTime
type Checker = NominalDiffTime -> Bool
type EdgeStateUpdater a = StateT EdgeState IO a

edgeKeys :: [((T.Text, T.Text), Checker)]
edgeKeys = [(("pieru", "perse"), mkChecker 1000), (("d", "c"), mkChecker 5000)]

builder :: EdgeChecker Bool -> Checker -> IO (Bool, UTCTime)
builder behaviour chk = do
  k <- getCurrentTime
  runStateT (runReaderT (runEC behaviour) chk) k

bap b xs = M.map (builder b) $ M.fromList xs

parseLineGetState :: M.Map (Text, Text) (IO (Bool, UTCTime)) -> T.Text -> Maybe (IO (Bool, UTCTime))
parseLineGetState m k = M.lookup (a, b) m
  where
    [a, b] = T.splitOn " " k

readGetState m = parseLineGetState m <$> fmap T.pack getLine

bjonn = bap currentTimeChecker edgeKeys

blart = do
  s <- readGetState bjonn
  case s of
    Just j -> do
      (p, k) <- j
      print $ show p ++ show k
      blart
    Nothing -> putStrLn "piss off" >> blart

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
main = blart

currentTimeChecker :: EdgeChecker Bool
currentTimeChecker = do
  time <- liftIO getCurrentTime
  trigger time 

perse :: EdgeChecker ()
perse = do
  _ <- liftIO getChar
  oldTime <- get
  newTime <- liftIO getCurrentTime
  pass <- trigger newTime
  let diff = diffUTCTime newTime oldTime
  liftIO . putStrLn $ "At " ++ show newTime ++ ": " ++ show pass ++ " delta: " ++ show diff
  perse
    
  
