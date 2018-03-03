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

-- chain :: MVar a -> (a -> b) -> MVar b
chain :: MVar a -> (a -> b) -> IO (MVar b) -- TODO: Functor? How? Async?
chain m f = async (f <$> await m)



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

type Cache = InMemoryDataSource

-- TODO: this design cannot work for parallel computations (intrinsically sequential)
data ProductionRepository a = ProductionRepository
    { runProduction :: Cache -> IO (MVar (a, Cache)) }

withProduction :: Cache -> ProductionRepository a -> IO a
withProduction db f = do
    var <- runProduction f db
    val <- await var
    pure (fst val)

instance Monad ProductionRepository where
    ra >>= f = ProductionRepository $ \db -> do
        var <- runProduction ra db
        (a, db') <- await var
        runProduction (f a) db'

instance Functor ProductionRepository where
    -- Chaining futures
    fmap f m = ProductionRepository $ \db -> do
        var <- runProduction m db
        chain var $ \(a, db) -> (f a, db)

instance Applicative ProductionRepository where
  pure a = ProductionRepository $ \db -> async $ pure (a, db)
  -- TODO: Do things in parallel while the Monad does things sequentially (join DBs?)
  (<*>) = ap

instance MonadIO ProductionRepository where
    liftIO io = ProductionRepository $ \db -> do
        a <- io
        async $ pure (a, db)

instance IRepository ProductionRepository where
    tradeVersions timelineId =
        ProductionRepository $ \db -> do
            case M.lookup timelineId (tradeVersions_ db) of
                Just transitions -> async $ pure (transitions, db)
                Nothing -> async $ do
                    transitions <- httpGetTradeVersions timelineId
                    pure (transitions, db { tradeVersions_ = M.insert timelineId transitions (tradeVersions_ db) })
    connectedTimelines timelineId =
        ProductionRepository $ \db ->
            async $ pure (M.findWithDefault [] timelineId (connectedTimelines_ db), db)
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
