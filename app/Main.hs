{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GADTs #-}

module Main where

import Prelude as P
import Data.String.Strip
import Data.Time.Clock
import Data.Ratio
import Data.List
import Control.Concurrent
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Text as T

data Event a = Event
  { timestamp :: UTCTime
  , eventId :: a
  } deriving (Eq, Show)

data Node = Node
  { label :: Text
  , history :: [Event Text]
  }

data Edge = Edge
  { src :: Text
  , dst :: Text
  , state :: [Event Text]
  }

data TimeUnit a where
  Seconds :: Int -> TimeUnit Int
  Milliseconds :: Int -> TimeUnit Int
  Microseconds :: Int -> TimeUnit Int

data Check = Temporal Int Int (TimeUnit Int) | Cardinal Int Int Int

toMicro :: TimeUnit Int -> TimeUnit Int
toMicro (Microseconds x) = Microseconds x
toMicro (Seconds x) = Microseconds (x * 100000)
toMicro (Milliseconds x) = Microseconds (x * 1000)

toDiffTime :: TimeUnit Int -> NominalDiffTime
toDiffTime (Microseconds t) = fromIntegral t
toDiffTime t = toDiffTime . toMicro $ t

data Status = OK | WARN | NOK

type Checker = Check -> Status

findPairs :: Node -> Node -> [(Event Text, Event Text)]
findPairs n1 n2 = do
  w1 <- history n1
  w2 <- history n2
  if eventId w1 == eventId w2
    then return (w1, w2)
    else []
  
correlate :: Edge -> Check -> Node -> Node -> Maybe Status
correlate edge checker n1 n2 = do
  guard $ src edge == label n1 && dst edge == label n2
  case checker of
    Temporal okThreshold warnThreshold window -> do
      let pairs = findPairs n1 n2
      guard $ P.length pairs > 0
      let successes = P.filter (\(e1, e2) -> diffUTCTime (timestamp e1) (timestamp e2) <= (toDiffTime window)) pairs
          failures = pairs \\ successes
      return $
        case (P.length successes, P.length failures) of
          (okThreshold, _) -> OK
          _                -> NOK
    Cardinal _ _ _ -> do
      return NOK

main = do
  putStrLn "hello"
