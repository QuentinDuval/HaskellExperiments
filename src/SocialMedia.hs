-- {-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
module SocialMedia(socialMediaTest) where

import Control.Concurrent
import Control.Monad
import Data.Function(on)
import Data.List
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
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
    await a = readMVar a

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

type ProfileId = Int
type Subject = String
type PostId = Int

data BlogPost = BlogPost -- Would add a postId, but useless here
  { postSubject :: Subject
  , postLikes :: Int
  } deriving (Show, Eq, Ord)

class Monad m => IRepository m where
  friendsOf :: ProfileId -> m [ProfileId]
  subjectsOf :: ProfileId -> m (Set Subject)
  lastPostsOf :: ProfileId -> m [BlogPost]

-- This code is embarded in a "onConnection" use case... you cannot just separate effects
getSuggestedPosts :: (IRepository m) => ProfileId -> m [BlogPost]
getSuggestedPosts userId = do
    friendIds <- friendsOf userId
    subjects <- subjectsOf userId
    friendsPosts <- forM friendIds $ \friendId -> do
        posts <- lastPostsOf friendId
        return (filter (isAbout subjects) posts)
    return $ mostLiked 3 (concat friendsPosts)

isAbout :: (Set Subject) -> BlogPost -> Bool
isAbout subjects subject = Set.member (postSubject subject) subjects

mostLiked :: Int -> [BlogPost] -> [BlogPost]
mostLiked n = take n . reverse . sortOn postLikes


--------------------------------------------------------------------------------
-- Infrastructure code (in memory)
--------------------------------------------------------------------------------

data InMemoryDataSource = InMemoryDataSource
    { friendIds_ :: Map ProfileId [ProfileId]
    , subjects_ :: Map ProfileId (Set Subject)
    , lastPosts_ :: Map ProfileId [BlogPost]
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
    friendsOf profileId = InMemoryRepository $ \db ->
        (Map.findWithDefault [] profileId (friendIds_ db), db)
    subjectsOf profileId = InMemoryRepository $ \db ->
        (Map.findWithDefault Set.empty profileId (subjects_ db), db)
    lastPostsOf profileId = InMemoryRepository $ \db ->
        (Map.findWithDefault [] profileId (lastPosts_ db), db)


--------------------------------------------------------------------------------
-- Infrastructure code (distance call for trade versions)
--------------------------------------------------------------------------------

data Cache = Cache
    { cachedFriendIds_ :: Map ProfileId [ProfileId]
    , cachedSubjects_  :: Map ProfileId (Set Subject)
    , cachedLastPosts_ :: Map ProfileId (MVar [BlogPost])
    }

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
        fVar <- async $ runProduction mf db -- TODO: mf may block... find another way?
        aVar <- async $ runProduction ma db
        async $ do
            f <- await =<< await fVar
            a <- await =<< await aVar
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
    friendsOf profileId = ProductionRepository $ \dbVar -> do
        db <- readMVar dbVar
        sync $ return $ Map.findWithDefault [] profileId (cachedFriendIds_ db)
    subjectsOf profileId = ProductionRepository $ \dbVar -> do
        db <- readMVar dbVar
        sync $ return $ Map.findWithDefault Set.empty profileId (cachedSubjects_ db)
    lastPostsOf profileId = ProductionRepository $ \dbVar ->
        fetchPosts dbVar profileId

fetchPosts :: MVar Cache -> ProfileId -> IO (MVar [BlogPost])
fetchPosts dbVar profileId = do
    db <- readMVar dbVar
    case Map.lookup profileId (cachedLastPosts_ db) of
        Just posts -> pure posts
        Nothing -> do
            -- Put the MVar in the map before completion to avoid multiple request to the same profileId
            transitions <- httpGetLastPosts profileId
            addInCache dbVar profileId transitions
            pure transitions

addInCache :: MVar Cache -> ProfileId -> MVar [BlogPost] -> IO ()
addInCache dbVar profileId posts = do
    db <- takeMVar dbVar
    putMVar dbVar $ db { cachedLastPosts_ = Map.insert profileId posts (cachedLastPosts_ db) }

httpGetLastPosts :: ProfileId -> IO (MVar [BlogPost])
httpGetLastPosts profileId = async $ do
    logInfo ("Query for posts of " ++ show profileId)
    liftIO (threadDelay 1000000)
    if profileId == 1 then
        return [BlogPost "C++" 15, BlogPost "Java" 10]
    else if profileId == 2 then
        return [BlogPost "Haskell" 15, BlogPost "C++" 5]
    else if profileId == 3 then
        return [BlogPost "Java" 20]
    else
        return []

--------------------------------------------------------------------------------
-- Test
--------------------------------------------------------------------------------

socialMediaTest :: IO ()
socialMediaTest = do
    let inMemoryDb = InMemoryDataSource
                      { friendIds_ = Map.fromList [(1, [1, 2, 3]), (2, [1, 2, 3]), (3, [1, 2, 3])]
                      , subjects_ = Map.fromList [ (1, Set.fromList["C++", "Java"])
                                                 , (2, Set.fromList["C++", "Haskell"])
                                                 , (3, Set.fromList["Clojure", "Haskell"])]
                      , lastPosts_ = Map.fromList [ (1, [BlogPost "C++" 15, BlogPost "Java" 10] )
                                                  , (2, [BlogPost "Haskell" 15, BlogPost "C++" 5] )
                                                  , (3, [BlogPost "Java" 20])
                                                  ]}
    print $ withInMemoryDb inMemoryDb $ do
        t1 <- getSuggestedPosts 1
        t2 <- getSuggestedPosts 2
        pure (t1, t2)

    let cache = Cache
                  { cachedFriendIds_ = friendIds_ inMemoryDb
                  , cachedSubjects_ = subjects_ inMemoryDb
                  , cachedLastPosts_ = Map.empty }
    withProduction cache $ do
        t1 <- getSuggestedPosts 1
        t2 <- getSuggestedPosts 2
        liftIO (print (t1, t2))

    -- To show that it does not reuse the same cache (we could however do it)
    withProduction cache $ do
        t1 <- getSuggestedPosts 2
        t2 <- getSuggestedPosts 3
        liftIO (print (t1, t2))
--
