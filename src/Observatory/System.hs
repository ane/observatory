{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Observatory.System where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.Reader
import Control.Monad.Trans.Maybe
import           Data.List
import qualified Data.Map.Strict      as M
import           Data.Maybe
import qualified Data.Text.Lazy       as T
import           Observatory.Edge
import           Observatory.Types

type SystemData = M.Map (Node, Node) EdgeState

data System = System
  { edges  :: SystemData
  , queue  :: TQueue NodeEvent
  }

newtype SystemM a = SysM
  { runSystemM :: ReaderT System IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader System)

data NodeEvent = NodeEvent
  { sourceName :: T.Text
  , targetName :: T.Text
  , edgeEvent  :: EdgeEvent } deriving Show

findEdgeBy :: (Node -> Node -> Bool) -> SystemM (Maybe EdgeState)
findEdgeBy f = do
  sys <- ask
  return $ listToMaybe $ take 1 $ M.elems $
    M.filterWithKey (\(s, t) _ -> f s t) (edges sys)

edgeM :: EdgeState -> EdgeM a -> STM a
edgeM state c = runReaderT c state

fromNodeEvent :: NodeEvent -> Node -> Node -> Bool
fromNodeEvent a n1 n2 = sourceName a == name n1 && targetName a == name n2

fromSrcDstPair :: T.Text -> T.Text -> Node -> Node -> Bool
fromSrcDstPair s d a b = s == name a && d == name b

withEdgeM :: (Node -> Node -> Bool) -> EdgeM a -> SystemM (STM a)
withEdgeM f c = do
  s <- findEdgeBy f
  case s of
    Just state -> return $ edgeM state c
    Nothing -> liftIO mzero

dispatch :: NodeEvent -> SystemM (STM ())
dispatch e = withEdgeM (fromNodeEvent e) $ update (edgeEvent e)

getStatus :: T.Text -> T.Text -> SystemM (STM (Maybe Status))
getStatus source destination = 
  withEdgeM (fromSrcDstPair source destination) status

enqueue :: NodeEvent -> SystemM (STM ())
enqueue ev = do
  sys <- ask
  return $ writeTQueue (queue sys) ev

runWorker :: System -> IO ThreadId
runWorker sys = forkIO $ do
  work <- atomically $ readTQueue (queue sys)
  loop work sys
 where
   loop work s = do
     putStrLn $ "Received " ++ show work
     action <- runReaderT (runSystemM $ dispatch work) s
     nxt <- atomically $ action >> readTQueue (queue s)
     loop nxt s
