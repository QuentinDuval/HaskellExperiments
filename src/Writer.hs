{-# LANGUAGE FlexibleInstances #-}
module Writer where

import Control.Monad


-- Writer Monad (no side effect, just accumulate)

data Writer m a
  = Writer { writerLog :: m, writerOut :: a }
  deriving (Show)

instance Functor (Writer m) where
  fmap f (Writer m a) = Writer m (f a)

instance (Monoid m) => Applicative (Writer m) where
  pure = Writer mempty
  Writer m1 f <*> Writer m2 a = Writer (mappend m1 m2) (f a)

instance (Monoid m) => Monad (Writer m) where
  Writer m1 a >>= f = let Writer m2 b = f a in Writer (mappend m1 m2) b

--

class Monad m => MonadLog m where
  logInfo :: String -> m ()

instance MonadLog (Writer String) where
  logInfo s = Writer (s ++ "\n") ()

--

type Snail = String

popSnail :: (MonadLog m) => String -> m Snail
popSnail name = do
  logInfo $ "New snail created: " ++ name
  pure name

sendDart :: (MonadLog m) => Snail -> Snail -> m Bool
sendDart s1 s2 = do
  logInfo $ "Snail " ++ s1 ++ " sends love dart to " ++ s2
  pure True

parentalAdvisorySnailStuff :: (MonadLog m) => Snail -> Snail -> m Bool
parentalAdvisorySnailStuff s1 s2 = do
  logInfo $ "Snails " ++ s1 ++ " and " ++ s2 ++ " just did it."
  pure True

andM :: (Monad m) => [m Bool] -> m Bool
andM = foldM (\res action -> if not res then pure res else action) True

mateSnails :: (MonadLog m) => m Bool
mateSnails = do
  slug <- popSnail "Slug"
  slim <- popSnail "Slim"
  andM [
    sendDart slug slim ,
    sendDart slim slug ,
    parentalAdvisorySnailStuff slug slim ]

{-
  ok1 <-
  ok2 <- sendDart slim slug
  ok3 <- parentalAdvisorySnailStuff slug slim
  pure (ok1 && ok2 && ok3)
-}

--

run_test :: IO ()
run_test = do
  putStrLn $ writerLog mateSnails

--
