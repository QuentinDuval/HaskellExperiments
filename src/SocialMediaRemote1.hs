{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs, RankNTypes, ConstraintKinds, FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric, DeriveAnyClass #-}
module SocialMediaRemote1 where

import Control.Concurrent
import Control.Monad
import Data.Function(on)
import Data.Hashable
import qualified Data.HashMap.Lazy as HashMap
import Data.HashMap.Lazy(HashMap)
import Data.List
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import qualified Data.Time.Clock as Clock
import System.IO.Unsafe(unsafePerformIO)
import Unsafe.Coerce

import SocialMedia
import SocialMediaInMemory


--------------------------------------------------------------------------------
-- Test
--------------------------------------------------------------------------------

test :: IO ()
test = withRemoteProfileInfo $ do
        t1 <- suggestedPostsFor 1
        t1' <- suggestedPostsFor 1
        t2 <- suggestedPostsFor 2
        liftIO (print (t1, t1', t2))


--------------------------------------------------------------------------------
-- Infrastructure code (Generic code)
--------------------------------------------------------------------------------

class Fetchable req where
  fetch :: req a -> IO (MVar a)

type Cachable req a = (Eq (req a), Hashable (req a))
type IRequest req a = (Fetchable req, Cachable req a)

newtype RequestCache req =
  RequestCache (forall a. HashMap (req a) (MVar a))

emptyCache :: RequestCache req
emptyCache = RequestCache HashMap.empty

lookupCache :: IRequest req a => req a -> RequestCache req -> Maybe (MVar a)
lookupCache key (RequestCache m) = HashMap.lookup key m

insertCache :: IRequest req a => req a -> MVar a -> RequestCache req -> RequestCache req
insertCache key val (RequestCache m) = RequestCache $ unsafeCoerce (HashMap.insert key val m)

--------------------------------------------------------------------------------

newtype AsyncFetch req a =
  AsyncFetch { runAsync :: MVar (RequestCache req) -> IO (MVar a) }

instance Functor (AsyncFetch req) where
  fmap f m = AsyncFetch $ \cache -> do
    var <- runAsync m cache
    chain var f

instance Applicative (AsyncFetch req) where
    pure a = AsyncFetch $ \cache -> sync (pure a)
    -- (<*>) = ap -- Without parallel procesing
    mf <*> ma = AsyncFetch $ \cache -> do
        fVar <- async (runAsync mf cache)
        aVar <- async (runAsync ma cache)
        async $ do
            f <- await =<< await fVar
            a <- await =<< await aVar
            pure (f a)

instance Monad (AsyncFetch req) where
    ra >>= f = AsyncFetch $ \cache -> do        -- Sequential processing needed
        a <- await =<< runAsync ra cache
        runAsync (f a) cache

instance MonadIO (AsyncFetch req) where
    liftIO io = AsyncFetch $ \cache -> sync io

select :: IRequest req a => req a -> AsyncFetch req a
select req = AsyncFetch $ \cacheVar -> do
  cache <- takeMVar cacheVar
  case lookupCache req cache of
    Just var -> do
      putMVar cacheVar cache
      pure var
    Nothing -> do
      var <- fetch req
      putMVar cacheVar (insertCache req var cache)
      pure var


--------------------------------------------------------------------------------
-- Instiation for our DSL
--------------------------------------------------------------------------------

withRemoteProfileInfo :: AsyncFetch ProfileRequest a -> IO a
withRemoteProfileInfo f = do
    cache <- newMVar emptyCache
    await =<< runAsync f cache

data ProfileRequest a where
  FriendsOf :: ProfileId -> ProfileRequest [ProfileId]
  FavoriteTopicsOf :: ProfileId -> ProfileRequest (Set Topic)
  LastPostsOf :: ProfileId -> ProfileRequest [BlogPost]

instance WithProfileInfo (AsyncFetch ProfileRequest) where
  friendsOf = select . FriendsOf
  favoriteTopicsOf = select . FavoriteTopicsOf
  lastPostsOf = select . LastPostsOf

deriving instance Eq (ProfileRequest a)

instance Hashable (ProfileRequest a) where
  hashWithSalt salt req = hashWithSalt salt (requestType req) + hashWithSalt salt (userId req) where
    requestType :: ProfileRequest a -> Int
    requestType (FriendsOf _) = 1
    requestType (FavoriteTopicsOf _) = 2
    requestType (LastPostsOf _) = 3

userId :: (ProfileRequest a) -> ProfileId
userId (FriendsOf userId) = userId
userId (FavoriteTopicsOf userId) = userId
userId (LastPostsOf userId) = userId

instance Fetchable ProfileRequest where
  fetch (FriendsOf profileId) = httpGetFriends profileId
  fetch (FavoriteTopicsOf profileId) = httpGetSubjects profileId
  fetch (LastPostsOf profileId) = httpGetLastPosts profileId

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
    t <- Clock.getCurrentTime
    putStrLn (show t ++ " - " ++ s)
    putMVar ioMutex ()

--
