{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
module SocialMedia where

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
-- Domain code
--------------------------------------------------------------------------------

type ProfileId = Int
type Topic = String
type PostId = Int

data BlogPost = BlogPost -- Would add a postId, but useless here
  { postSubject :: Topic
  , postLikes :: Int
  } deriving (Show, Eq, Ord)

class Monad m => WithProfileInfo m where
  friendsOf :: ProfileId -> m [ProfileId]
  favoriteTopicsOf :: ProfileId -> m (Set Topic)
  lastPostsOf :: ProfileId -> m [BlogPost]

suggestedPostsFor :: (WithProfileInfo m) => ProfileId -> m [BlogPost]
suggestedPostsFor userId = do
    friendIds <- friendsOf userId
    topics <- favoriteTopicsOf userId
    friendsPosts <- forM friendIds $ \friendId -> do
        posts <- lastPostsOf friendId
        return (filter (isAbout topics) posts)
    return (mostLiked 3 (concat friendsPosts))

isAbout :: (Set Topic) -> BlogPost -> Bool
isAbout topics topic = Set.member (postSubject topic) topics

mostLiked :: Int -> [BlogPost] -> [BlogPost]
mostLiked n = take n . reverse . sortOn postLikes


--------------------------------------------------------------------------------
-- Two use cases:
-- 1. We ask for the suggestion (REST API) => we search in DB first or call suggestedPostsFor
-- 2. We ask for reset (JMS queue) => we call suggestedPostsFor and replace the suggestions
--------------------------------------------------------------------------------

class Monad m => WithSuggestionDB m where
  saveSuggestion :: ProfileId -> [BlogPost] -> m ()
  loadSuggestion :: ProfileId -> m [BlogPost]

getSuggestedPosts :: (WithProfileInfo m, WithSuggestionDB m) => ProfileId -> m [BlogPost]
getSuggestedPosts userId = do
    suggestions <- loadSuggestion userId
    if not (null suggestions) then
        return suggestions
    else do
        suggestions <- suggestedPostsFor userId -- TODO: load suggestion OR - to avoid the race
        saveSuggestion userId suggestions
        return suggestions

onResetMessage :: (WithSuggestionDB m) => ProfileId -> m ()
onResetMessage userId =
    saveSuggestion userId []

--
