{-# LANGUAGE FlexibleInstances #-}
module Writer where

import Control.Monad
import Control.Monad.Identity
import Data.Text.Lazy
import Data.Text.Lazy.Builder


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

write :: m -> Writer m ()
write m = Writer m ()

--

class Monad m => MonadLog m where
  logInfo :: String -> m ()

instance MonadLog (Writer Builder) where
  logInfo s = write $ fromString (s ++ "\n")

instance MonadLog (Writer [Text]) where
  logInfo s = write [pack s]

instance MonadLog IO where
  logInfo s = print s

instance MonadLog Identity where
  logInfo _ = pure ()

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

orM :: (Monad m) => [m Bool] -> m Bool
orM = foldM (\res action -> if res then pure res else action) False

mateSnails :: (MonadLog m) => m Bool
mateSnails = do
  slug <- popSnail "Slug"
  slim <- popSnail "Slim"
  andM [
    sendDart slug slim , -- TODO: in C++, just wrap in lambda and return pair(std::string, result)?
    sendDart slim slug , -- TODO: in C++, just wrap in lambda and still use log interface
    parentalAdvisorySnailStuff slug slim ]

{-
  ok1 <-
  ok2 <- sendDart slim slug
  ok3 <- parentalAdvisorySnailStuff slug slim
  pure (ok1 && ok2 && ok3)
-}

-- Discarding the logs

data Proxy a

withoutLogs :: Proxy m -> Writer m a -> a
withoutLogs _ = writerOut

runNoLogs :: Identity a -> a
runNoLogs = runIdentity

discardLogs :: (Monoid m) => Writer m a -> Writer m a
discardLogs w = w { writerLog = mempty }

-- dropLogs :: Writer (could you write this???)

class MonadLog m => MonadLogTry m where
  tryLog :: m (a, Bool) -> m a

instance MonadLogTry (Writer Builder) where
  tryLog (Writer m (a, keep))
    | keep = Writer m a
    | otherwise = Writer mempty a

tryMates :: (MonadLogTry m) => m Bool
tryMates =
  orM [
    tryLog $ fmap (\_ -> (False, False)) $ mateSnails ,
    tryLog $ fmap (\_ -> (True, True)) $ mateSnails ,
    mateSnails ]

-- TODO: can you still memoize it once it is abstract? (Memo Monad?)

--

run_test :: IO ()
run_test = do
  print $ writerLog (mateSnails :: Writer Builder Bool)
  mapM_ print $ writerLog (mateSnails :: Writer [Text] Bool)
  mateSnails

  print $ writerLog (discardLogs mateSnails :: Writer Builder Bool)
  print $ withoutLogs (undefined :: Proxy Builder) mateSnails
  print $ runNoLogs mateSnails
  print $ writerLog (tryMates :: Writer Builder Bool)

--
