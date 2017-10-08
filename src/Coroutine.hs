{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
module Coroutine where

import Control.Monad.Par


-- Goal:
-- * implement a small non-blocking Monad to do async calls to external entity
-- * make sure you can test it by executing it synchronously

class Monad m => Coroutine m where
  type Future m :: * -> *
  async :: (NFData a) => m a -> m (Future m a)
  await :: Future m a -> m a

instance Coroutine Par where
  type Future Par = IVar
  async = spawn
  await = get

-- co_await :: Coroutine m => m (Future m a) -> m a
-- co_await fut = fut >>= await

----

type Film = String
type Year = Int

fetchFilm :: Monad m => Int -> m Film
fetchFilm _ = pure "Film"

getYear :: Film -> Year
getYear = length

fetchAtSameYear :: Monad m => Year -> m [Film]
fetchAtSameYear y = pure (replicate y "c")

countFilms :: Coroutine m => Int -> m (Future m Int)
countFilms filmId = do
  film <- async (fetchFilm filmId)
  year <- getYear <$> await film
  films <- async (fetchAtSameYear year)
  async $ fmap length (await films)

test_par_counts :: IO (Int, Int)
test_par_counts = runParIO $ do
  a <- countFilms 1
  b <- countFilms 2
  (,) <$> await a <*> await b

--
