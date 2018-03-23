module SocialMediaInMemory where

import Control.Monad
import Data.List
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set

import SocialMedia


--------------------------------------------------------------------------------
-- Test
--------------------------------------------------------------------------------

inMemoryDb :: LocalProfileInfo
inMemoryDb = LocalProfileInfo
              { friendIds_ = Map.fromList [ (1, [2, 3])
                                          , (2, [1, 3])
                                          , (3, [1, 2]) ]
              , subjects_ = Map.fromList [ (1, Set.fromList["C++", "Java"])
                                         , (2, Set.fromList["C++", "Haskell"])
                                         , (3, Set.fromList["Clojure", "Haskell"])]
              , lastPosts_ = Map.fromList [ (1, [BlogPost "C++" 15, BlogPost "Java" 10] )
                                          , (2, [BlogPost "Haskell" 15, BlogPost "C++" 5, BlogPost "C++" 10] )
                                          , (3, [BlogPost "Java" 20, BlogPost "Haskell" 5, BlogPost "Java" 5])
                                          ]
              , suggestions_ = Map.empty }


test :: IO ()
test = do
  print $ withInMemoryDb inMemoryDb $ do
      t1 <- getSuggestedPosts 1
      t1' <- getSuggestedPosts 1
      t2 <- getSuggestedPosts 2
      pure (t1, t1', t2)


--------------------------------------------------------------------------------
-- Monad implementation
--------------------------------------------------------------------------------

data LocalProfileInfo = LocalProfileInfo
    { friendIds_ :: Map ProfileId [ProfileId]
    , subjects_ :: Map ProfileId (Set Topic)
    , lastPosts_ :: Map ProfileId [BlogPost]
    , suggestions_ :: Map ProfileId [BlogPost]
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
    favoriteTopicsOf profileId = InMemoryRepository $ \db ->
        (Map.findWithDefault Set.empty profileId (subjects_ db), db)
    lastPostsOf profileId = InMemoryRepository $ \db ->
        (Map.findWithDefault [] profileId (lastPosts_ db), db)

instance WithSuggestionDB InMemoryRepository where
    saveSuggestion userId posts = InMemoryRepository $ \db ->
        ((), db { suggestions_ = Map.insert userId posts (suggestions_ db) })
    loadSuggestion userId = InMemoryRepository $ \db ->
        (Map.findWithDefault [] userId (suggestions_ db), db)
