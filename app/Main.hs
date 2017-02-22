{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import           Control.Concurrent
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Data.List
import           Data.Ratio
import           Data.String.Strip
import qualified Data.Text                 as T
import           Data.Time.Clock
import           Prelude                   as P

data Event = Event
  { timestamp :: UTCTime
  , eventId   :: T.Text
  }

data Node = Node
  { label   :: T.Text
  , history :: [Event]
  }

data Edge = Edge
  { src   :: T.Text
  , dst   :: T.Text
  , state :: [Event]
  }

data TimeUnit a where
  Seconds :: Int -> TimeUnit Int
  Milliseconds :: Int -> TimeUnit Int
  Microseconds :: Int -> TimeUnit Int

data Check = Temporal (TimeUnit Int) Int Int Int

toMicro :: TimeUnit Int -> TimeUnit Int
toMicro (Microseconds x) = Microseconds x
toMicro (Seconds x)      = Microseconds (x * 100000)
toMicro (Milliseconds x) = Microseconds (x * 1000)

toDiffTime :: TimeUnit Int -> NominalDiffTime
toDiffTime (Microseconds t) = fromIntegral t
toDiffTime t                = toDiffTime . toMicro $ t

data Status = OK | WARN | NOK

type Checker = Check -> Status

findPairs :: Node -> Node -> (Event -> Event -> Bool) -> [(Event, Event)]
findPairs n1 n2 eq = do
  e1 <- history n1
  e2 <- history n2
  if eq e1 e2 then return (e1, e2) else []

runChecker :: Check -> Node -> Node -> Maybe Status
runChecker checker n1 n2 = do
  let pairs = findPairs n1 n2 (\e1 e2 -> eventId e1 == eventId e2)
  guard $ not (null pairs)
  return $
    case checker of
      Temporal win ok warn fail ->
        case (length passing, length failing) of
          (_, fc) | fail >= fc -> NOK
          (_, fc) | warn >= fc -> WARN
          (sc, _) | ok   >= sc -> OK
        where
          (passing, failing) = partition (\(e1, e2) -> toDiffTime win >= diffUTCTime (timestamp e1) (timestamp e2)) pairs

main = do
  putStrLn "hello"

