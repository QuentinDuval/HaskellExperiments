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

class Monad m => WithProfileInfo m where
  friendsOf :: ProfileId -> m [ProfileId]
  subjectsOf :: ProfileId -> m (Set Subject)
  lastPostsOf :: ProfileId -> m [BlogPost]

-- This code is embarded in a "onConnection" use case... you cannot just separate effects
-- TODO: save the suggested posts... and check it at the beginning... in context, would be reset by other notifications
getSuggestedPosts :: (WithProfileInfo m) => ProfileId -> m [BlogPost]
getSuggestedPosts userId = do
    friendIds <- friendsOf userId
    subjects <- subjectsOf userId
    friendsPosts <- forM friendIds $ \friendId -> do
        posts <- lastPostsOf friendId
        return (filter (isAbout subjects) posts)
    return (mostLiked 3 (concat friendsPosts))

isAbout :: (Set Subject) -> BlogPost -> Bool
isAbout subjects subject = Set.member (postSubject subject) subjects

mostLiked :: Int -> [BlogPost] -> [BlogPost]
mostLiked n = take n . reverse . sortOn postLikes


--------------------------------------------------------------------------------
-- Infrastructure code (in memory)
--------------------------------------------------------------------------------

data LocalProfileInfo = LocalProfileInfo
    { friendIds_ :: Map ProfileId [ProfileId]
    , subjects_ :: Map ProfileId (Set Subject)
    , lastPosts_ :: Map ProfileId [BlogPost]
    } deriving (Show, Eq, Ord)

-- With such a threading of the state, no parallelization is possible
data InMemoryRepository a = InMemoryRepository
    { runInMemory :: LocalProfileInfo -> (a, LocalProfileInfo) }

withInMemoryDb :: LocalProfileInfo -> InMemoryRepository a -> a
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

instance WithProfileInfo InMemoryRepository where
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
    { cachedFriendIds_ :: Map ProfileId (MVar [ProfileId])
    , cachedSubjects_  :: Map ProfileId (MVar (Set Subject))
    , cachedLastPosts_ :: Map ProfileId (MVar [BlogPost])
    }

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
    subjectsOf profileId = RemoteProfileInfo $ \dbVar -> fetchSubjects dbVar profileId
    lastPostsOf profileId = RemoteProfileInfo $ \dbVar -> fetchLastPosts dbVar profileId

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

fetchSubjects :: MVar Cache -> ProfileId -> IO (MVar (Set Subject)) -- TODO: use lens to factorize
fetchSubjects dbVar profileId = do
    db <- takeMVar dbVar
    (subjects, cachedSubjects) <- fetchAndCache (cachedSubjects_ db) profileId httpGetSubjects
    putMVar dbVar $ db { cachedSubjects_ = cachedSubjects }
    return subjects

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
    return $ Map.findWithDefault [] profileId (friendIds_ inMemoryDb)

httpGetSubjects :: ProfileId -> IO (MVar (Set Subject))
httpGetSubjects profileId = async $ do
    logInfo ("Query for subjects of " ++ show profileId)
    liftIO (threadDelay 1000000)
    return $ Map.findWithDefault Set.empty profileId (subjects_ inMemoryDb)

httpGetLastPosts :: ProfileId -> IO (MVar [BlogPost])
httpGetLastPosts profileId = async $ do
    logInfo ("Query for posts of " ++ show profileId)
    liftIO (threadDelay 1000000)
    return $ Map.findWithDefault [] profileId (lastPosts_ inMemoryDb)


--------------------------------------------------------------------------------
-- Test
--------------------------------------------------------------------------------

inMemoryDb :: LocalProfileInfo
inMemoryDb = LocalProfileInfo
              { friendIds_ = Map.fromList [ (1, [1, 2, 3])
                                          , (2, [1, 2, 3])
                                          , (3, [1, 2, 3]) ]
              , subjects_ = Map.fromList [ (1, Set.fromList["C++", "Java"])
                                         , (2, Set.fromList["C++", "Haskell"])
                                         , (3, Set.fromList["Clojure", "Haskell"])]
              , lastPosts_ = Map.fromList [ (1, [BlogPost "C++" 15, BlogPost "Java" 10] )
                                          , (2, [BlogPost "Haskell" 15, BlogPost "C++" 5] )
                                          , (3, [BlogPost "Java" 20, BlogPost "Haskell" 5])
                                          ]}

socialMediaTest :: IO ()
socialMediaTest = do
    print $ withInMemoryDb inMemoryDb $ do
        t1 <- getSuggestedPosts 1
        t2 <- getSuggestedPosts 2
        pure (t1, t2)

    let cache = Cache { cachedFriendIds_ = Map.empty, cachedSubjects_ = Map.empty, cachedLastPosts_ = Map.empty }
    withRemoteProfileInfo cache $ do
        t1 <- getSuggestedPosts 1
        t2 <- getSuggestedPosts 2
        liftIO (print (t1, t2))
    withRemoteProfileInfo cache $ do -- with a new cache
        t1 <- getSuggestedPosts 2
        t2 <- getSuggestedPosts 3
        liftIO (print (t1, t2))
--
