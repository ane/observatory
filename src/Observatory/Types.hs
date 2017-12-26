{-# LANGUAGE GADTs #-}

module Observatory.Types where

import           Data.Text.Lazy  as T
import           Data.Time.Clock

data Node = Node { label :: T.Text }

data Event = Event
  { eventAt :: UTCTime
  , eventId :: T.Text
  }

data Edge = Edge
  { okThreshold   :: Int
  , warnThreshold :: Int
  , failThreshold :: Int
  }

class Timestamped a where
  timestamp :: a -> UTCTime

data EdgeEvent =
    BasicEvent { success :: Bool, at :: UTCTime }
  | TemporalEvent { duration :: NominalDiffTime, when :: UTCTime }

instance Timestamped EdgeEvent where
  timestamp (BasicEvent _ x)    = x
  timestamp (TemporalEvent _ x) = x

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
