{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs, RankNTypes, ConstraintKinds, FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric, DeriveAnyClass #-}
module SocialMediaRemote2 where

import Data.IORef
import Data.Hashable
import qualified Data.HashMap.Lazy as HashMap
import Data.HashMap.Lazy(HashMap)
import qualified Data.Set
import Data.Set(Set)
import GHC.Generics
import Unsafe.Coerce

import SocialMedia


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

deriving instance Eq (ProfileRequest a)

instance Hashable (ProfileRequest a) where
  hashWithSalt salt req@(FriendsOf userId) = hashWithSalt salt (requestType req) + hashWithSalt salt userId
  hashWithSalt salt req@(FavoriteTopicsOf userId) = hashWithSalt salt (requestType req) + hashWithSalt salt userId
  hashWithSalt salt req@(LastPostsOf userId) = hashWithSalt salt (requestType req) + hashWithSalt salt userId

requestType :: ProfileRequest a -> ProfileRequestType
requestType (FriendsOf _) = FriendsOfRequest
requestType (FavoriteTopicsOf _) = FavoriteTopicsOfRequest
requestType (LastPostsOf _) = LastPostsOfRequest

-- This is one choice of implementation... in fact, if some requests on the DSL side can be
-- served by a single request on the imlementation side, we can do something even different
-- CHECK the imlementation of OM Next (that does DECLARATIVE queries as well)
instance Fetchable ProfileRequest where
  fetch requests =
    let categories = HashMap.fromListWith (++) $ map (\r@(BlockedRequest req _) -> (requestType req, [r])) requests
    in undefined

instance WithProfileInfo (BulkFetch ProfileRequest) where
  friendsOf = select . FriendsOf
  favoriteTopicsOf = select . FavoriteTopicsOf
  lastPostsOf = select . LastPostsOf

--
