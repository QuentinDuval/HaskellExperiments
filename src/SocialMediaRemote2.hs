{-# LANGUAGE RankNTypes, ConstraintKinds #-}
module SocialMediaRemote2 where

import Data.IORef
import Data.Hashable
import qualified Data.HashMap.Lazy as Map
import Data.HashMap.Lazy(HashMap)
import qualified Data.Set
import Data.Set(Set)
import Unsafe.Coerce

import SocialMedia


--------------------------------------------------------------------------------
-- Test
--------------------------------------------------------------------------------

test :: IO ()
test = pure ()


--------------------------------------------------------------------------------
-- Free Monad implementation
--------------------------------------------------------------------------------

type IRequest req a = (Eq (req a), Hashable (req a))

data RequestStatus a = Success a | Processing

data BlockedRequest req where
  BlockedRequest :: req a -> IORef (RequestStatus a) -> BlockedRequest req

newtype DataCache req =
  DataCache (forall a. HashMap (req a) (IORef (RequestStatus a)))

data Result req a
  = Done a
  | Blocked [BlockedRequest req] (Fetch req a)

newtype Fetch req a =
  Fetch { unFetch :: IORef (DataCache req) -> IO (Result req a) }

lookupCache :: IRequest req a => req a -> DataCache req -> Maybe (IORef (RequestStatus a))
lookupCache key (DataCache m) = Map.lookup key m

insertCache :: IRequest req a => req a -> IORef (RequestStatus a) -> DataCache req -> DataCache req
insertCache key val (DataCache m) =
  DataCache $ unsafeCoerce (Map.insert key val m)

dataFetch :: IRequest req a => req a -> Fetch req a
dataFetch req = Fetch $ \ref -> do
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
    cont box = Fetch $ \ref -> do
      Success a <- readIORef box
      return (Done a)


--------------------------------------------------------------------------------
-- Application
--------------------------------------------------------------------------------

data ProfileRequest a where
  FriendsOf :: ProfileId -> ProfileRequest [ProfileId]
  FavoriteTopicsOf :: ProfileId -> ProfileRequest (Set Topic)
  LastPostsOf :: ProfileId -> ProfileRequest [BlogPost]


--
