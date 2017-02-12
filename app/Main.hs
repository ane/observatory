{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
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

type EdgeState = [UTCTime]
type Checker = NominalDiffTime -> Bool
type EdgeStateUpdater a = StateT EdgeState IO a

newtype EdgeChecker a = EdgeChecker {
    runEC :: ReaderT Checker (StateT EdgeState IO) a
    } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Checker, MonadState EdgeState)

edgeKeys :: [((T.Text, T.Text), Checker)]
edgeKeys = [(("pieru", "perse"), mkChecker 1000), (("d", "c"), mkChecker 5000)]

           
builder behaviour chk = do
  x <- getCurrentTime
  execStateT (runReaderT (runEC behaviour) chk) [x]

bap b xs = M.map (builder b) $ M.fromList xs

parseLineGetState :: Ord k => M.Map k (EdgeState) -> k -> Maybe EdgeState
parseLineGetState m k = M.lookup k m

readGetState :: (Text, Text) -> Map (Text, Text) EdgeState -> Maybe EdgeState
readGetState k m = parseLineGetState m k

bjonn :: IO (Map (Text, Text) EdgeState)
bjonn = mapM (builder currentTimeChecker) (M.fromList edgeKeys)

runChecker :: (Text, Text) -> EdgeState -> IO (Bool, EdgeState)
runChecker (s, d) old = runStateT (runReaderT (runEC currentTimeChecker) (mkChecker 1000)) old
  
blart m = do
  [src, dst] <- (T.splitOn " " . T.pack) `fmap` getLine
  b <- m
  case readGetState (src, dst) b of
    Just oldState -> do
      (pass, newState) <- runChecker (src, dst) oldState
      putStrLn $ "Diff to previous: " ++ show (diffUTCTime (Prelude.head newState) (Prelude.head oldState))
      putStrLn $ show pass ++ ", history: " ++ show newState
      blart $ return (M.insert (src, dst) newState b)
    Nothing -> do
      putStrLn "fail!"
      blart $ return b

trigger :: UTCTime -> EdgeChecker Bool
trigger time = do
  checker <- ask
  times <- get
  modify (time:)
  return $ checker $ diffUTCTime time (Prelude.head times)

mkChecker :: Integer -> NominalDiffTime -> Bool
mkChecker diff = (>=) (fromRational (diff % 1000) :: NominalDiffTime)

main = blart bjonn

currentTimeChecker :: EdgeChecker Bool
currentTimeChecker = do
  time <- liftIO getCurrentTime
  trigger time 

perse :: EdgeChecker ()
perse = do
  _ <- liftIO getChar
  oldTimes <- get
  newTime <- liftIO getCurrentTime
  pass <- trigger newTime
  let diff = diffUTCTime newTime (Prelude.head oldTimes)
  liftIO . putStrLn $ "At " ++ show newTime ++ ": " ++ show pass ++ " delta: " ++ show diff
  perse
    
  
