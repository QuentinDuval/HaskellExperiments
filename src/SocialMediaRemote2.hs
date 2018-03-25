{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs, RankNTypes, ConstraintKinds, FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric, DeriveAnyClass #-}
module SocialMediaRemote2 where

import Control.Concurrent
import Control.Monad
import Data.IORef
import Data.Hashable
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.HashMap.Lazy as HashMap
import Data.HashMap.Lazy(HashMap)
import qualified Data.Set as Set
import Data.Set(Set)
import GHC.Generics
import System.IO.Unsafe(unsafePerformIO)
import Unsafe.Coerce

import SocialMedia
import SocialMediaInMemory


--------------------------------------------------------------------------------
-- Test
--------------------------------------------------------------------------------

test :: IO ()
test = do
    r <- withBulkProfileInfo $ do
      t1 <- suggestedPostsFor 1
      t1' <- suggestedPostsFor 1
      t2 <- suggestedPostsFor 2
      pure (t1, t1', t2)
    print r

withBulkProfileInfo :: BulkFetch ProfileRequest a -> IO a
withBulkProfileInfo = withBulkRequests


--------------------------------------------------------------------------------
-- Free Monad implementation
--------------------------------------------------------------------------------

data RequestStatus a -- For the caching indicate an already running request
  = Success a
  | Processing

-- The IORef is only there to give to the fetch method below...
-- So that the implementation does not manipulate the cache
data BlockedRequest req where
  BlockedRequest :: req a -> IORef (RequestStatus a) -> BlockedRequest req

class Fetchable req where
  fetch :: [BlockedRequest req] -> IO ()

type Cachable req a = (Eq (req a), Hashable (req a))
type IRequest req a = (Fetchable req, Cachable req a)

newtype RequestCache req =
  RequestCache (forall a. HashMap (req a) (IORef (RequestStatus a)))

emptyCache :: RequestCache req
emptyCache = RequestCache HashMap.empty

lookupCache :: IRequest req a => req a -> RequestCache req -> Maybe (IORef (RequestStatus a))
lookupCache key (RequestCache m) = HashMap.lookup key m

insertCache :: IRequest req a => req a -> IORef (RequestStatus a) -> RequestCache req -> RequestCache req
insertCache key val (RequestCache m) = RequestCache $ unsafeCoerce (HashMap.insert key val m)

--------------------------------------------------------------------------------

data Result req a
  = Done a
  | Blocked [BlockedRequest req] (BulkFetch req a)

newtype BulkFetch req a =
  BulkFetch { runBulk :: IORef (RequestCache req) -> IO (Result req a) }

instance Functor (BulkFetch req) where
  fmap f (BulkFetch g) = BulkFetch $ \cache -> do
    a <- g cache
    case a of
      Done a -> pure (Done (f a))
      Blocked br c -> pure (Blocked br (fmap f c))

instance Applicative (BulkFetch req) where
  pure a = BulkFetch $ \cache -> pure (Done a)
  BulkFetch f <*> BulkFetch x = BulkFetch $ \cache -> do
    f' <- f cache
    x' <- x cache
    case (f', x') of
      (Done g, Done y ) -> pure (Done (g y))
      (Done g, Blocked br c ) -> pure (Blocked br (g <$> c))
      (Blocked br c, Done y ) -> pure (Blocked br (c <*> pure y))
      (Blocked br1 c, Blocked br2 d) -> pure (Blocked (br1 ++ br2) (c <*> d))

instance Monad (BulkFetch req) where
  BulkFetch m >>= k = BulkFetch $ \cache -> do
    r <- m cache
    case r of
      Done a -> runBulk (k a) cache
      Blocked br c -> return (Blocked br (c >>= k))

withBulkRequests :: (IRequest req a) => BulkFetch req a -> IO a
withBulkRequests f = do
    cache <- newIORef emptyCache
    loop cache f
  where
    loop cache f = do
      r <- runBulk f cache
      case r of
        Done a -> return a
        Blocked br cont -> do
          fetch br
          loop cache cont

--------------------------------------------------------------------------------

-- ^ Main method used create a request on the client side
select :: IRequest req a => req a -> BulkFetch req a
select req = BulkFetch $ \ref -> do
  cache <- readIORef ref
  case lookupCache req cache of
    Nothing -> do
      box <- newIORef Processing
      writeIORef ref (insertCache req box cache)
      let br = BlockedRequest req box
      return (Blocked [br] (cont box))
    Just box -> do
      r <- readIORef box
      case r of
        Success result -> return (Done result)
        Processing -> return (Blocked [] (cont box))
  where
    cont box = BulkFetch $ \ref -> do
      Success a <- readIORef box
      return (Done a)


--------------------------------------------------------------------------------
-- Application
--------------------------------------------------------------------------------

data ProfileRequestType
  = FriendsOfRequest
  | FavoriteTopicsOfRequest
  | LastPostsOfRequest
  deriving (Show, Eq, Ord, Generic, Hashable)

data ProfileRequest a where
  FriendsOf :: ProfileId -> ProfileRequest [ProfileId]
  FavoriteTopicsOf :: ProfileId -> ProfileRequest (Set Topic)
  LastPostsOf :: ProfileId -> ProfileRequest [BlogPost]

instance WithProfileInfo (BulkFetch ProfileRequest) where
  friendsOf = select . FriendsOf
  favoriteTopicsOf = select . FavoriteTopicsOf
  lastPostsOf = select . LastPostsOf

deriving instance Eq (ProfileRequest a)

instance Hashable (ProfileRequest a) where
  hashWithSalt salt req = hashWithSalt salt (requestType req) + hashWithSalt salt (userId req)

userId :: (ProfileRequest a) -> ProfileId
userId (FriendsOf userId) = userId
userId (FavoriteTopicsOf userId) = userId
userId (LastPostsOf userId) = userId

requestType :: ProfileRequest a -> ProfileRequestType
requestType (FriendsOf _) = FriendsOfRequest
requestType (FavoriteTopicsOf _) = FavoriteTopicsOfRequest
requestType (LastPostsOf _) = LastPostsOfRequest

-- This is one choice of implementation... in fact, if some requests on the DSL side can be
-- served by a single request on the imlementation side, we can do something even different
-- CHECK the imlementation of OM Next (that does DECLARATIVE queries as well)
instance Fetchable ProfileRequest where
  fetch requests = do
    let categories = Map.fromListWith (++) $ map (\r@(BlockedRequest req _) -> (requestType req, [r])) requests
    let idsByCategories = (fmap . fmap) (\(BlockedRequest req _) -> userId req) categories

    -- TODO: this is bad, doing a request still
    friendsAnswers <- async $ httpGetFriends $ Map.findWithDefault [] FriendsOfRequest idsByCategories
    topicsAnswers <- async $ httpGetTopics $ Map.findWithDefault [] FavoriteTopicsOfRequest idsByCategories
    lastPostsAnswers <- httpGetLastPosts $ Map.findWithDefault [] LastPostsOfRequest idsByCategories

    friendsAnswers <- await friendsAnswers
    topicsAnswers <- await topicsAnswers
    forM_ requests (fillRequest friendsAnswers topicsAnswers lastPostsAnswers)

fillRequest :: HashMap ProfileId [ProfileId]
              -> HashMap ProfileId (Set Topic)
              -> HashMap ProfileId [BlogPost]
              -> BlockedRequest ProfileRequest -> IO ()
fillRequest friendsAnswers topicsAnswers lastPostsAnswers (BlockedRequest request ret) =
  case request of
    FriendsOf userId -> writeIORef ret (Success (friendsAnswers HashMap.! userId))
    FavoriteTopicsOf userId -> writeIORef ret (Success (topicsAnswers HashMap.! userId))
    LastPostsOf userId -> writeIORef ret (Success (lastPostsAnswers HashMap.! userId))


--------------------------------------------------------------------------------
-- Implementation of the requests
--------------------------------------------------------------------------------

httpGetFriends :: [ProfileId] -> IO (HashMap ProfileId [ProfileId])
httpGetFriends [] = return HashMap.empty
httpGetFriends profileIds = do
    logInfo ("Query for friends of " ++ show profileIds)
    threadDelay 1000000
    logInfo ("Got friends of " ++ show profileIds)
    let answers = map (\profileId -> Map.findWithDefault [] profileId (friendIds_ inMemoryDb)) profileIds
    return $ HashMap.fromList (zip profileIds answers)

httpGetTopics :: [ProfileId] -> IO (HashMap ProfileId (Set Topic))
httpGetTopics [] = return HashMap.empty
httpGetTopics profileIds = do
    logInfo ("Query for topics of " ++ show profileIds)
    threadDelay 1000000
    logInfo ("Got topics of " ++ show profileIds)
    let answers = map (\profileId -> Map.findWithDefault Set.empty profileId (subjects_ inMemoryDb)) profileIds
    return $ HashMap.fromList (zip profileIds answers)

httpGetLastPosts :: [ProfileId] -> IO (HashMap ProfileId [BlogPost])
httpGetLastPosts [] = return HashMap.empty
httpGetLastPosts profileIds = do
    logInfo ("Query for posts of " ++ show profileIds)
    threadDelay 1000000
    logInfo ("Got posts of " ++ show profileIds)
    let answers = map (\profileId -> Map.findWithDefault [] profileId (lastPosts_ inMemoryDb)) profileIds
    return $ HashMap.fromList (zip profileIds answers)


--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

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

ioMutex :: MVar ()
{-# NOINLINE ioMutex #-}
ioMutex = unsafePerformIO (newMVar ())

logInfo :: String -> IO ()
logInfo s = do
    takeMVar ioMutex
    putStrLn s
    putMVar ioMutex ()

--
