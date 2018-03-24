{-# LANGUAGE ApplicativeDo #-}
module SocialMediaRemote1 where

import Control.Concurrent
import Control.Monad
import Data.Function(on)
import Data.List
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import System.IO.Unsafe(unsafePerformIO)

import SocialMedia
import SocialMediaInMemory


--------------------------------------------------------------------------------
-- Test
--------------------------------------------------------------------------------

test :: IO ()
test = withRemoteProfileInfo emptyCache $ do
        t1 <- getSuggestedPosts 1
        t1' <- getSuggestedPosts 1
        t2 <- getSuggestedPosts 2
        liftIO (print (t1, t1', t2))


--------------------------------------------------------------------------------
-- Infrastructure code (distance call for trade versions)
--------------------------------------------------------------------------------

data Cache = Cache
    { cachedFriendIds_ :: Map ProfileId (MVar [ProfileId])
    , cachedSubjects_  :: Map ProfileId (MVar (Set Topic))
    , cachedLastPosts_ :: Map ProfileId (MVar [BlogPost])
    , savedSuggestions_ :: Map ProfileId [BlogPost]
    }

emptyCache :: Cache
emptyCache = Cache { cachedFriendIds_ = Map.empty
                   , cachedSubjects_ = Map.empty
                   , cachedLastPosts_ = Map.empty
                   , savedSuggestions_ = Map.empty }

-- The MVar is needed VS state monad to avoid sequential applicatives
data RemoteProfileInfo a = RemoteProfileInfo
    { runRemoteProfileInfo :: MVar Cache -> IO (MVar a) }

withRemoteProfileInfo :: Cache -> RemoteProfileInfo a -> IO a
withRemoteProfileInfo db f = do
    dbVar <- newMVar db
    await =<< runRemoteProfileInfo f dbVar

instance Functor RemoteProfileInfo where
    fmap f m = RemoteProfileInfo $ \db -> do -- Chaining futures
        var <- runRemoteProfileInfo m db
        chain var f

instance Applicative RemoteProfileInfo where
    pure a = RemoteProfileInfo $ \db -> sync (pure a)
    -- (<*>) = ap -- Without parallel procesing
    mf <*> ma = RemoteProfileInfo $ \db -> do       -- With parallel processing
        fVar <- async $ runRemoteProfileInfo mf db  -- TODO: mf may block! find another way?
        aVar <- async $ runRemoteProfileInfo ma db
        async $ do
            f <- await =<< await fVar
            a <- await =<< await aVar
            pure (f a)

instance Monad RemoteProfileInfo where
    ra >>= f = RemoteProfileInfo $ \db -> do        -- Sequential processing needed
        a <- await =<< runRemoteProfileInfo ra db
        runRemoteProfileInfo (f a) db

instance MonadIO RemoteProfileInfo where
    liftIO io = RemoteProfileInfo $ \db -> sync io

instance WithProfileInfo RemoteProfileInfo where
    friendsOf profileId = RemoteProfileInfo $ \dbVar -> fetchFriends dbVar profileId
    favoriteTopicsOf profileId = RemoteProfileInfo $ \dbVar -> fetchSubjects dbVar profileId
    lastPostsOf profileId = RemoteProfileInfo $ \dbVar -> fetchLastPosts dbVar profileId

instance WithSuggestionDB RemoteProfileInfo where
    saveSuggestion userId posts = RemoteProfileInfo $ \dbVar -> sync $ do
        logInfo ("Save suggestion " ++ show userId)
        db <- takeMVar dbVar
        putMVar dbVar $ db { savedSuggestions_ = Map.insert userId posts (savedSuggestions_ db) }
    loadSuggestion userId = RemoteProfileInfo $ \dbVar -> sync $ do
        logInfo ("Load suggestion " ++ show userId)
        db <- readMVar dbVar
        return $ Map.findWithDefault [] userId (savedSuggestions_ db)

fetchAndCache :: (Ord k) => Map k (MVar v) -> k -> (k -> IO (MVar v)) -> IO (MVar v, Map k (MVar v))
fetchAndCache cache key f =
    case Map.lookup key cache of
        Just posts -> pure (posts, cache)
        Nothing -> do
            result <- f key -- Put MVar in cache before completion (avoid multiple request to same key)
            pure (result, Map.insert key result cache)

--------------------------------------------------------------------------------

fetchFriends :: MVar Cache -> ProfileId -> IO (MVar [ProfileId]) -- TODO: use lens to factorize
fetchFriends dbVar profileId = do
    db <- takeMVar dbVar
    (friends, cachedFriends) <- fetchAndCache (cachedFriendIds_ db) profileId httpGetFriends
    putMVar dbVar $ db { cachedFriendIds_ = cachedFriends }
    return friends

fetchSubjects :: MVar Cache -> ProfileId -> IO (MVar (Set Topic)) -- TODO: use lens to factorize
fetchSubjects dbVar profileId = do
    db <- takeMVar dbVar
    (topics, cachedSubjects) <- fetchAndCache (cachedSubjects_ db) profileId httpGetSubjects
    putMVar dbVar $ db { cachedSubjects_ = cachedSubjects }
    return topics

fetchLastPosts :: MVar Cache -> ProfileId -> IO (MVar [BlogPost]) -- TODO: use lens to factorize
fetchLastPosts dbVar profileId = do
    db <- takeMVar dbVar
    (posts, cachedPosts) <- fetchAndCache (cachedLastPosts_ db) profileId httpGetLastPosts
    putMVar dbVar $ db { cachedLastPosts_ = cachedPosts }
    return posts

httpGetFriends :: ProfileId -> IO (MVar [ProfileId])
httpGetFriends profileId = async $ do
    logInfo ("Query for friends of " ++ show profileId)
    liftIO (threadDelay 1000000)
    logInfo ("Got friends of " ++ show profileId)
    return $ Map.findWithDefault [] profileId (friendIds_ inMemoryDb)

httpGetSubjects :: ProfileId -> IO (MVar (Set Topic))
httpGetSubjects profileId = async $ do
    logInfo ("Query for topics of " ++ show profileId)
    liftIO (threadDelay 1000000)
    logInfo ("Got topics of " ++ show profileId)
    return $ Map.findWithDefault Set.empty profileId (subjects_ inMemoryDb)

httpGetLastPosts :: ProfileId -> IO (MVar [BlogPost])
httpGetLastPosts profileId = async $ do
    logInfo ("Query for posts of " ++ show profileId)
    liftIO (threadDelay 1000000)
    logInfo ("Got posts of " ++ show profileId)
    return $ Map.findWithDefault [] profileId (lastPosts_ inMemoryDb)

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

chain :: MVar a -> (a -> b) -> IO (MVar b) -- Functor instance... requires Async
chain m f = async (f <$> await m)

ioMutex :: MVar ()
{-# NOINLINE ioMutex #-}
ioMutex = unsafePerformIO (newMVar ())

logInfo :: (MonadIO m) => String -> m ()
logInfo s = liftIO $ do
    takeMVar ioMutex
    putStrLn s
    putMVar ioMutex ()

--
