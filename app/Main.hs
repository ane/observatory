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

data Event a = Event
  { timestamp :: UTCTime
  , id :: a
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

data Check = Temporal Int Int | Cardinal Int Int Int

data Status = OK | WARN | NOK

type Checker = Check -> Status

main = do
  putStrLn "hello"
  
