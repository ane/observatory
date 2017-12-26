{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Observatory.Edge where

import           Control.Concurrent.STM
import           Control.Monad.Reader
import qualified Data.Sequence          as S
import           Observatory.Types

data EdgeState = EdgeConfig
  { kind    :: EdgeKind
  , config  :: Edge
  , history :: TVar (S.Seq EdgeEvent)
  }

newtype EdgeM a = EdgeM
  { runEdgeM :: ReaderT EdgeState IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader EdgeState)

data Status = OK | Warn | Fail

partition :: EdgeM (Int, Int)
partition = do
  state <- ask
  hist <- liftIO $ readTVarIO $ history state
  return $
    foldl (\(s,f) event ->
            case event of
              BasicEvent succeeded _ ->
                if succeeded then (s + 1, f) else (s, f + 1)
              TemporalEvent dur _ ->
                case kind state of
                  BasicEdge -> (s + 1, f)
                  TemporalEdge w ->
                    if toDiffTime w >= dur then (s+1, f) else (s, f + 1))
         (0, 0) -- why is my code staring at me?
         hist

getHistory :: EdgeM (S.Seq EdgeEvent)
getHistory = ask >>= \s -> liftIO $ readTVarIO $ history s

minimumNeeded :: EdgeState -> Int
minimumNeeded state =
  let cfg = config state in
    okThreshold cfg + warnThreshold cfg + failThreshold cfg

update :: EdgeEvent -> EdgeM (STM ())
update event = do
  state <- ask
  return $ 
    modifyTVar (history state) $
      \events ->
        if length events == minimumNeeded state
          then S.drop 1 events S.|> event
          else events S.|> event

status :: EdgeM (Maybe Status)
status = do
  state <- ask
  (_, f) <- partition
  hist <- liftIO $ readTVarIO $ history state
  let eventCount = length hist
      needed = minimumNeeded state
      c = config state
  return $ do
    guard $ eventCount >= needed
    return $
      if f >= failThreshold c
        then Fail
        else if f >= warnThreshold c
          then Warn
          else OK




