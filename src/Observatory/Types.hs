{-# LANGUAGE GADTs #-}

module Observatory.Types where

import           Data.Text.Lazy  as T
import           Data.Time.Clock

data Node = Node { name :: T.Text } deriving (Eq, Ord, Show)

data Event = Event
  { eventAt :: UTCTime
  , eventId :: T.Text
  } deriving Show

data Edge = Edge
  { okThreshold   :: Int
  , warnThreshold :: Int
  , failThreshold :: Int
  }

data EdgeEvent =
    BasicEvent { success :: Bool, base :: Event }
  | TemporalEvent { duration :: NominalDiffTime, base :: Event }
  deriving Show

data EdgeKind = BasicEdge | TemporalEdge { window :: TimeUnit Int }

data TimeUnit a where
  Seconds :: Int -> TimeUnit Int
  Milliseconds :: Int -> TimeUnit Int
  Microseconds :: Int -> TimeUnit Int

toMicro :: TimeUnit Int -> TimeUnit Int
toMicro (Microseconds x) = Microseconds x
toMicro (Seconds x)      = Microseconds (x * 1000000)
toMicro (Milliseconds x) = Microseconds (x * 1000)

toDiffTime :: TimeUnit Int -> NominalDiffTime
toDiffTime (Microseconds t) = fromIntegral t
toDiffTime t                = toDiffTime . toMicro $ t
