{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Observatory.Edge where

import           Control.Concurrent.STM
import           Control.Monad.Reader
import qualified Data.Sequence          as S
import           Observatory.Types

data EdgeState = EdgeState
  { kind    :: EdgeKind
  , config  :: Edge
  , history :: TVar (S.Seq EdgeEvent)
  }

type EdgeM a = ReaderT EdgeState STM a

data Status = OK | Warn | Fail deriving Show

partition :: ReaderT EdgeState STM (Int, Int)
partition = do
  state <- ask
  lift $ do
    hist <- readTVar $ history state
    return $ foldl (\(s,f) event ->
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
getHistory = ask >>= \s -> lift $ readTVar $ history s

minimumNeeded :: EdgeState -> Int
minimumNeeded state =
  let cfg = config state in
    okThreshold cfg + warnThreshold cfg + failThreshold cfg

update :: EdgeEvent -> EdgeM ()
update event = do
  state <- ask
  lift $ modifyTVar' (history state) $
    \events ->
      if length events == minimumNeeded state
        then S.drop 1 events S.|> event
        else events S.|> event

status :: EdgeM (Maybe Status)
status = do
  state <- ask
  (_, f) <- partition
  hist <- lift $ readTVar $ history state
  let eventCount = length hist
      needed = minimumNeeded state
      c = config state
    -- guard $ eventCount >= needed
  return $
    if eventCount >= needed
    then Just $ 
           if f >= failThreshold c
           then Fail
           else if f >= warnThreshold c
                then Warn
                else OK
    else Nothing




