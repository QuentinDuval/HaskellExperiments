{-# LANGUAGE FlexibleInstances, ConstraintKinds #-}
module WriterT where

import Control.Arrow(second)
import Control.Monad
import Control.Monad.Identity
import Data.Text.Lazy
import Data.Text.Lazy.Builder


-- This is in the end very similar to:
-- * Dependency injection of interface with log function
-- * Implementation in terms of accumulating to strings
-- * But more declarative (and potentially memoizable?)


-- Writer Monad (no side effect, just accumulate)

data WriterT m w a = WriterT { runWriterT :: m (w, a) }

instance (Functor m) => Functor (WriterT m w) where
  fmap f = WriterT . fmap (second f) . runWriterT

{-
instance (Applicative m, Monoid w) => Applicative (WriterT m w) where
  pure = WriterT mempty . pure
  WriterT w1 f <*> WriterT w2 a = WriterT (mappend w1 w2) (f <*> a)

instance (Monad m, Monoid w) => Monad (WriterT m w) where
  WriterT w1 a >>= f =
    let WriterT w2 b = f a
    in WriterT (mappend w1 w2) b

write :: Applicative m => w -> WriterT m w ()
write m = WriterT m (pure ())
-}

--

{-

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

data Proxy a = Proxy

withoutLogs :: Proxy m -> Writer m a -> a
withoutLogs _ = writerOut

runNoLogs :: Identity a -> a
runNoLogs = runIdentity

discardLogs :: (Monoid m) => Writer m a -> Writer m a
discardLogs w = w { writerLog = mempty }

-- Now to the advantage of Data
-- * You can drop the logs (side effects cannot be rollbacked)
-- * You can memoize the logs! (TODO - How? Not inside the abstraction, after)
-- * You can serialize it (leads to memo... TODO is it the way to implement memo?)

class Monad m => MonadTry m where
  tryDo :: m (a, Bool) -> m a -- TODO: dropLog instead... build try on top

type MonadLogTry m = (MonadLog m, MonadTry m)

instance (Monoid m) => MonadTry (Writer m) where
  tryDo (Writer m (a, keep))
    | keep = Writer m a
    | otherwise = Writer mempty a

tryMates :: (MonadLogTry m) => m Bool
tryMates =
  orM [
    tryDo $ fmap (\_ -> (False, False)) $ mateSnails ,
    tryDo $ fmap (\_ -> (True, True)) $ mateSnails ,
    mateSnails ]

--

run_test :: IO ()
run_test = do
  print $ writerLog (mateSnails :: Writer Builder Bool)
  mapM_ print $ writerLog (mateSnails :: Writer [Text] Bool)
  mateSnails

  print $ writerLog (discardLogs mateSnails :: Writer Builder Bool)
  print $ withoutLogs (Proxy :: Proxy Builder) mateSnails
  print $ runNoLogs mateSnails
  print $ writerLog (tryMates :: Writer Builder Bool)
-}

--
