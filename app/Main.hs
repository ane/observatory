{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GADTs #-}

module Main where

import Prelude as P
import Data.String.Strip
import Data.Time.Clock
import Data.Ratio
import Control.Concurrent
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Text as T

data Event a = Event
  { timestamp :: UTCTime
  , eventId :: a
  }

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

data Check = Temporal Int (TimeUnit Int) | Cardinal Int Int Int

toMicro :: TimeUnit Int -> TimeUnit Int
toMicro (Microseconds x) = Microseconds x
toMicro (Seconds x) = Microseconds (x * 100000)
toMicro (Milliseconds x) = Microseconds (x * 1000)

toDiffTime :: TimeUnit Int -> NominalDiffTime
toDiffTime (Microseconds t) = fromIntegral t
toDiffTime t = toDiffTime . toMicro $ t

data Status = OK | WARN | NOK

type Checker = Check -> Status

findCorrelation :: Check -> Node -> Node -> [(Event Text, Event Text)]
findCorrelation c1 n1 n2 =
  let h1 = history n1
      h2 = history n2
      last1 = P.head h1
      last2 = P.head h2
      withinOne = P.filter (eventMatcher last1) h1
      withinTwo = P.filter (eventMatcher last2) h2
  in
  do w1 <- withinOne
     w2 <- withinTwo
     if eventId w1 == eventId w2 then return (w1, w2) else []
  where
    eventMatcher :: Event Text -> Event Text -> Bool
    eventMatcher last e =
      case c1 of
        Temporal _ unit -> let dt = toDiffTime unit in diffUTCTime (timestamp last) (timestamp e) <= dt
        Cardinal _ _ _  -> False
  
buildChecker :: Edge -> Check -> Node -> Node -> Maybe Status
buildChecker edge checker n1 n2 = 
  do m1 <- if src edge == label n1 then Just n1 else Nothing
     m2 <- if dst edge == label n2 then Just n2 else Nothing
     return $
       case checker of
         Temporal count unit -> OK
         Cardinal{} -> NOK

main = do
  putStrLn "hello"
  
