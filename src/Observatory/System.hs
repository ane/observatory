{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Observatory.System where

import           Control.Concurrent
import           Control.Monad.Reader
import           Data.Map           as M
import           Observatory.Edge
import           Observatory.Types

data System = System
  { edges  :: M.Map (Node, Node) EdgeState,
    worker :: IO ThreadId
  }

newtype SystemM a = SysM
  { runSystemM :: ReaderT System IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader System)
