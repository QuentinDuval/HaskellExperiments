module SocialMediaImperative where

import Control.Concurrent
import Control.Monad
import Data.List
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import qualified Data.Time.Clock as Clock

import SocialMedia
import SocialMediaInMemory

--

test :: IO ()
test = do
    r <- withImperative $ do
      t1 <- suggestedPostsFor 1
      t1' <- suggestedPostsFor 1
      t2 <- suggestedPostsFor 2
      return (t1, t1', t2)
    print r

withImperative :: ImperativeFetch a -> IO a
withImperative = runImperative

--

data ImperativeFetch a = ImperativeFetch { runImperative :: IO a }

instance Monad ImperativeFetch where
    return a = ImperativeFetch $ pure a
    ra >>= f = ImperativeFetch $ do
      a <- runImperative ra
      runImperative (f a)

instance Functor ImperativeFetch where
    fmap f pa = pa >>= pure . f

instance Applicative ImperativeFetch where
    pure = return
    (<*>) = ap

instance WithProfileInfo ImperativeFetch where
  friendsOf profileId = ImperativeFetch $ httpGetFriends profileId
  favoriteTopicsOf profileId = ImperativeFetch $ httpGetSubjects profileId
  lastPostsOf profileId = ImperativeFetch $ httpGetLastPosts profileId

--

httpGetFriends :: ProfileId -> IO [ProfileId]
httpGetFriends profileId = do
    logInfo ("Query for friends of " ++ show profileId)
    threadDelay 1000000
    logInfo ("Got friends of " ++ show profileId)
    return $ Map.findWithDefault [] profileId (friendIds_ inMemoryDb)

httpGetSubjects :: ProfileId -> IO (Set Topic)
httpGetSubjects profileId = do
    logInfo ("Query for topics of " ++ show profileId)
    threadDelay 1000000
    logInfo ("Got topics of " ++ show profileId)
    return $ Map.findWithDefault Set.empty profileId (subjects_ inMemoryDb)

httpGetLastPosts :: ProfileId -> IO [BlogPost]
httpGetLastPosts profileId = do
    logInfo ("Query for posts of " ++ show profileId)
    threadDelay 1000000
    logInfo ("Got posts of " ++ show profileId)
    return $ Map.findWithDefault [] profileId (lastPosts_ inMemoryDb)

--

logInfo :: String -> IO ()
logInfo s = do
    t <- Clock.getCurrentTime
    putStrLn (show t ++ " - " ++ s)

--
