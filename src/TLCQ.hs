-- {-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
module TLCQ (
    tlcqTests
) where

import Control.Concurrent
import Control.Monad
import qualified Data.Map as M

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

class Monad m => MonadIO m where
    liftIO :: IO a -> m a

instance MonadIO IO where
    liftIO = id

class Monad m => IAsync m where
    async :: m a -> m (MVar a)
    await :: MVar a -> m a

instance IAsync IO where
    async io = do
        mvar <- newEmptyMVar
        forkIO (io >>= putMVar mvar)
        pure mvar
    await a = takeMVar a


--------------------------------------------------------------------------------
-- Domain code
--------------------------------------------------------------------------------

type TimelineId = Int
type VersionId = String

data Transition = Transition
  { transitionFrom :: VersionId
  , transitionTo :: VersionId
  } deriving (Show, Eq, Ord)

class Monad m => IRepository m where
  tradeVersions :: TimelineId -> m [Transition]
  connectedTimelines :: TimelineId -> m [TimelineId]
  insert :: [Transition] -> m () -- not used here, but hey, you get the drill

type GenealogicTree = M.Map VersionId [VersionId]

genealogicTree :: (IRepository m) => TimelineId -> m GenealogicTree
genealogicTree timelineId = do
    timelineIds <- connectedTimelines timelineId
    transitions <- concat <$> traverse tradeVersions timelineIds
    pure (buildTree transitions)

buildTree :: [Transition] -> GenealogicTree
buildTree = foldl addEdge M.empty where
    addEdge graph Transition{..} =
        M.insertWith (++) transitionFrom [transitionTo] graph


--------------------------------------------------------------------------------
-- Infrastructure code (in memory)
--------------------------------------------------------------------------------

data InMemoryDataSource = InMemoryDataSource
    { connectedTimelines_ :: M.Map TimelineId [TimelineId]
    , tradeVersions_ :: M.Map TimelineId [Transition]
    } deriving (Show, Eq, Ord)

data InMemoryRepository a = InMemoryRepository
    { runInMemory :: InMemoryDataSource -> (a, InMemoryDataSource) }

withInMemoryDb :: InMemoryDataSource -> InMemoryRepository a -> a
withInMemoryDb db f = fst $ runInMemory f db

instance Monad InMemoryRepository where
    return a = InMemoryRepository $ \db -> (a, db)
    ra >>= f = InMemoryRepository $ \db ->
        let (a, db') = runInMemory ra db
        in runInMemory (f a) db'

instance Functor InMemoryRepository where
    fmap f pa = pa >>= pure . f

instance Applicative InMemoryRepository where
    pure = return
    (<*>) = ap

instance IRepository InMemoryRepository where
    tradeVersions timelineId =
        InMemoryRepository $ \db -> (M.findWithDefault [] timelineId (tradeVersions_ db), db)
    connectedTimelines timelineId =
        InMemoryRepository $ \db -> (M.findWithDefault [] timelineId (connectedTimelines_ db), db)
    insert = undefined -- TODO


--------------------------------------------------------------------------------
-- Infrastructure code (distance call for trade versions)
--------------------------------------------------------------------------------

httpGetTradeVersions :: MonadIO m => TimelineId -> m [Transition]
httpGetTradeVersions timelineId = do
    liftIO (putStrLn $ "Query for timeline " ++ show timelineId)
    liftIO (threadDelay 500000)
    if timelineId == 1 then
        return [Transition "a1" "b1", Transition "b1" "c"]
    else if timelineId == 2 then
        return [Transition "a2" "b2", Transition "b2" "c"]
    else if timelineId == 3 then
        return [Transition "c" "d1"]
    else
        return []

-- TODO: where the in memory is a cache for the trade versions
data ProductionRepository a = ProductionRepository
    { runProduction :: InMemoryDataSource -> IO (a, InMemoryDataSource) }

withProduction :: InMemoryDataSource -> ProductionRepository a -> IO a
withProduction db f = fst <$> runProduction f db

instance Monad ProductionRepository where
    return a = ProductionRepository $ \db -> pure (a, db)
    ra >>= f = ProductionRepository $ \db -> do
        (a, db') <- runProduction ra db
        runProduction (f a) db'

instance Functor ProductionRepository where
    fmap f pa = pa >>= pure . f

instance Applicative ProductionRepository where
    pure = return
    (<*>) = ap      -- TODO : Do things in parallel! (and sequentially in the Monad - with continuation - and show in main that it does not block)

instance MonadIO ProductionRepository where
    liftIO io = ProductionRepository $ \db -> do
        a <- io
        pure (a, db)

instance IRepository ProductionRepository where
    tradeVersions timelineId =
        ProductionRepository $ \db -> do
            case M.lookup timelineId (tradeVersions_ db) of
                Just transitions -> pure (transitions, db)
                Nothing -> do
                    transitions <- httpGetTradeVersions timelineId
                    pure (transitions, db { tradeVersions_ = M.insert timelineId transitions (tradeVersions_ db) })
    connectedTimelines timelineId =
        ProductionRepository $ \db -> pure (M.findWithDefault [] timelineId (connectedTimelines_ db), db)
    insert = undefined -- TODO



--------------------------------------------------------------------------------
-- Test
--------------------------------------------------------------------------------

tlcqTests :: IO ()
tlcqTests = do
    let inMemoryDb = InMemoryDataSource
                      { connectedTimelines_ = M.fromList [(1, [1, 2, 3]), (2, [1, 2, 3]), (3, [1, 2, 3])]
                      , tradeVersions_ = M.fromList [ (1, [Transition "a1" "b1", Transition "b1" "c"] )
                                                    , (2, [Transition "a2" "b2", Transition "b2" "c"] )
                                                    , (3, [Transition "c" "d1"])
                                                    ]}
    print $ withInMemoryDb inMemoryDb (genealogicTree 1)

    let cache = InMemoryDataSource { connectedTimelines_ = connectedTimelines_ inMemoryDb, tradeVersions_ = M.fromList [] }
    withProduction cache $ do
        t1 <- genealogicTree 1
        t2 <- genealogicTree 2
        liftIO (print (t1, t2))
    withProduction cache $ do
        t1 <- genealogicTree 2
        t2 <- genealogicTree 3
        liftIO (print (t1, t2))

--
