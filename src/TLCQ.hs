-- {-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
module TLCQ (
    tlcqTests
) where

import Control.Concurrent
import Control.Monad
import qualified Data.Map as M
import System.IO.Unsafe(unsafePerformIO)

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

class Monad m => MonadIO m where
    liftIO :: IO a -> m a

instance MonadIO IO where
    liftIO = id

class Monad m => IAsync m where
    sync :: m a -> m (MVar a)
    async :: m a -> m (MVar a)
    await :: MVar a -> m a

instance IAsync IO where
    sync io = io >>= newMVar
    async io = do
        mvar <- newEmptyMVar
        forkIO (io >>= putMVar mvar)
        pure mvar
    await a = takeMVar a

-- chain :: MVar a -> (a -> b) -> MVar b
chain :: MVar a -> (a -> b) -> IO (MVar b) -- TODO: Functor? How? Async?
chain m f = async (f <$> await m)

ioMutex :: MVar ()
{-# NOINLINE ioMutex #-}
ioMutex = unsafePerformIO (newMVar ())

logInfo :: (MonadIO m) => String -> m ()
logInfo s = liftIO $ do
    takeMVar ioMutex
    putStrLn s
    putMVar ioMutex ()


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

-- With such a threading of the state, no parallelization is possible
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
    tradeVersions timelineId = InMemoryRepository $ \db ->
        (M.findWithDefault [] timelineId (tradeVersions_ db), db)
    connectedTimelines timelineId = InMemoryRepository $ \db ->
        (M.findWithDefault [] timelineId (connectedTimelines_ db), db)
    insert = undefined -- TODO


--------------------------------------------------------------------------------
-- Infrastructure code (distance call for trade versions)
--------------------------------------------------------------------------------

type Cache = InMemoryDataSource

-- The MVar is needed VS state monad to avoid sequential applicatives
data ProductionRepository a = ProductionRepository
    { runProduction :: MVar Cache -> IO (MVar a) }

withProduction :: Cache -> ProductionRepository a -> IO a
withProduction db f = do
    dbVar <- newMVar db
    var <- runProduction f dbVar
    await var

instance Functor ProductionRepository where
    -- Chaining futures
    fmap f m = ProductionRepository $ \db -> do
        var <- runProduction m db
        chain var f

instance Applicative ProductionRepository where
    pure a = ProductionRepository $ \db -> sync (pure a)
    -- Without parallel procesing
    -- (<*>) = ap
    -- With parallel processing
    mf <*> ma = ProductionRepository $ \db -> do
        fVar <- runProduction mf db
        aVar <- runProduction ma db
        async $ do
            f <- await fVar
            a <- await aVar
            pure (f a)

instance Monad ProductionRepository where
    -- Sequential processing needed
    ra >>= f = ProductionRepository $ \db -> do
        var <- runProduction ra db
        a <- await var
        runProduction (f a) db

instance MonadIO ProductionRepository where
    liftIO io = ProductionRepository $ \db -> sync io

instance IRepository ProductionRepository where
    tradeVersions timelineId = ProductionRepository $ \dbVar ->
        fetchTradeVersions dbVar timelineId
    connectedTimelines timelineId = ProductionRepository $ \dbVar -> do
        db <- readMVar dbVar
        let timelines = M.findWithDefault [] timelineId (connectedTimelines_ db)
        sync (pure timelines)
    insert = undefined -- TODO

fetchTradeVersions :: MVar Cache -> TimelineId -> IO (MVar [Transition])
fetchTradeVersions dbVar timelineId = do
    db <- readMVar dbVar
    case M.lookup timelineId (tradeVersions_ db) of
        Just transitions -> sync (pure transitions)
        Nothing -> async $ do
            transitions <- httpGetTradeVersions timelineId
            addInCache dbVar timelineId transitions
            pure transitions

addInCache :: MVar Cache -> TimelineId -> [Transition] -> IO ()
addInCache dbVar timelineId transitions = do
    db <- takeMVar dbVar
    putMVar dbVar $
        db { tradeVersions_ = M.insert timelineId transitions (tradeVersions_ db) }

httpGetTradeVersions :: MonadIO m => TimelineId -> m [Transition]
httpGetTradeVersions timelineId = do
    logInfo ("Query for timeline " ++ show timelineId)
    liftIO (threadDelay 1000000)
    if timelineId == 1 then
        return [Transition "a1" "b1", Transition "b1" "c"]
    else if timelineId == 2 then
        return [Transition "a2" "b2", Transition "b2" "c"]
    else if timelineId == 3 then
        return [Transition "c" "d1"]
    else
        return []


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
    print $ withInMemoryDb inMemoryDb $ do
        t1 <- genealogicTree 1
        t2 <- genealogicTree 2
        pure (t1, t2)

    let cache = InMemoryDataSource { connectedTimelines_ = connectedTimelines_ inMemoryDb, tradeVersions_ = M.fromList [] }
    withProduction cache $ do
        t1 <- genealogicTree 1
        t2 <- genealogicTree 2
        liftIO (print (t1, t2))

    -- To show that it does not reuse the same cache (we could however do it)
    withProduction cache $ do
        t1 <- genealogicTree 2
        t2 <- genealogicTree 3
        liftIO (print (t1, t2))

--
