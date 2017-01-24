{-# LANGUAGE DeriveFunctor #-}
module FreeMonad where

-- import Control.Monad.Free
import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Data.List (tails)
import qualified Data.Map as Map

-- DSL
-- http://underscore.io/blog/posts/2015/04/14/free-monads-are-simple.html
-- http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html

{-
  TODO - Look at:
  https://hackage.haskell.org/package/QuickCheck Random testing of program properties
  https://hackage.haskell.org/package/lucid Constructing HTML
  https://hackage.haskell.org/package/opaleye Typed database access
  https://hackage.haskell.org/package/parsec Constructing parsers
  https://hackage.haskell.org/package/pipes Streaming applications
-}

--------------------------------------------------------------------------------
-- FREE MONAD:
-- It is like the Fix point for the catamorphism but with Pure and r
-- It is only there to help you define your Monad... you do not need it
--------------------------------------------------------------------------------

data Free f r
  = Free (f (Free f r))
  | Pure r

instance Functor f => Functor (Free f) where
  fmap g (Pure r) = Pure (g r)                -- One level deep (leaf)
  fmap g (Free f) = Free ((fmap . fmap) g f)  -- Two level deep (recursion)

instance Functor f => Applicative (Free f) where
  pure              = Pure
  Pure g <*> Pure a = Pure (g a)
  Pure g <*> Free f = Free ((fmap . fmap) g f)
  Free f <*> a      = Free (fmap (<*> a) f)

instance Functor f => Monad (Free f) where
  return = pure
  Pure a >>= g = g a
  Free f >>= g = Free (fmap (>>= g) f)

liftF :: Functor f => f r -> Free f r
liftF x = Free (fmap Pure x) -- Wraps each leaf with Pure

unliftF :: Monad f => Free f a -> f a
unliftF (Pure a) = return a
unliftF (Free a) = a >>= unliftF

--------------------------------------------------------------------------------
-- Our DSL 2
--------------------------------------------------------------------------------

data Request cont
  = ReadLine (String -> cont)
  | WriteLine String cont

instance Functor Request where
  fmap f (ReadLine g)     = ReadLine (f . g)
  fmap f (WriteLine s g)  = WriteLine s (f g)

type Program = Free Request

interpret :: Program r -> IO r -- Going from one Monad to another
interpret (Pure r) = return r
interpret (Free (ReadLine f)) = getLine >>= \l -> interpret (f l)
interpret (Free (WriteLine s f)) = putStrLn s >> interpret f

-- Helpers to build our language

readLine :: Program String
readLine = liftF (ReadLine id)

prnLine :: String -> Program String
prnLine s = liftF (WriteLine s s)

-- Our sample language

prog1 :: Program ()
prog1 = do
  r <- readLine
  forM_ (tails r) prnLine
  return ()


--------------------------------------------------------------------------------
-- Our DSL 2
--------------------------------------------------------------------------------

type MarketDataId = Int
type MarketData = String

data EvalStmt a
  = ReadMarket MarketDataId (MarketData -> a)
  deriving (Functor)

type Eval = Free EvalStmt
type FakeSource = Map.Map MarketDataId MarketData

getMarket :: MarketDataId -> Eval MarketData
getMarket mdsId = liftF (ReadMarket mdsId id)

runFakeEval :: Eval r -> Reader FakeSource r
runFakeEval (Pure r) = return r
runFakeEval (Free (ReadMarket mdsId f)) = do
  mds <- asks (Map.! mdsId)
  runFakeEval (f mds)

--------------------------------------------------------------------------------
-- Program 2
--------------------------------------------------------------------------------

prog2 :: Eval [MarketData]
prog2 = do
  r1 <- getMarket 1
  r2 <- getMarket 2
  return [r1, r2]

testProg2 :: IO ()
testProg2 = do
  let m = Map.fromList [(1, "EUR"), (2, "USD")]
  print $ runReader (runFakeEval prog2) m


--------------------------------------------------------------------------------
-- Program 3
--------------------------------------------------------------------------------

data ProcessR cont
  = Get (String -> cont)
  | Put String cont
  | Fork (Free ProcessR ()) cont
  deriving (Functor)

type Process = Free ProcessR

put :: String -> Process ()
put s = liftF (Put s ())

get :: Process String
get = liftF (Get id)

fork :: Process () -> Process ()
fork p = liftF (Fork p ())

runProcess :: Process a -> IO a
runProcess (Free (Put msg p))  = putStrLn msg >> runProcess p
runProcess (Free (Get cont))   = getLine >>= \msg -> runProcess (cont msg)
runProcess (Free (Fork p1 p2)) = runProcess p1 >> runProcess p2 -- Sequential
runProcess (Pure x)            = pure x

runConcurrent :: Process a -> IO a -- bad things happen here (no sync on putStrLn)
runConcurrent (Free (Put msg p))  = putStrLn msg >> runConcurrent p
runConcurrent (Free (Get cont))   = getLine >>= \msg -> runConcurrent (cont msg)
runConcurrent (Free (Fork p1 p2)) = forkIO (runConcurrent p1) >> runConcurrent p2
runConcurrent (Pure x)            = pure x

{-
data Process a
  = Get (String -> Process a)
  | Put String (Process a)
  | Fork (Process ()) (Process a)
  | Done a
  deriving (Functor)

instance Applicative Process where
  pure = Done
  (<*>) = ap

instance Monad Process where
  return = pure
  (Put msg p) >>= f  = Put msg (p >>= f)
  (Get cont) >>= f   = Get $ \msg -> cont msg >>= f
  (Fork p1 p2) >>= f = Fork p1 (p2 >>= f)
  (Done x) >>= f     = f x

put :: String -> Process ()
put s = Put s (Done ())

get :: Process String
get = Get return

fork :: Process () -> Process ()
fork p = Fork p (Done ())

runProcess :: Process a -> IO a
runProcess (Put msg p)  = putStrLn msg >> runProcess p
runProcess (Get cont)   = getLine >>= \msg -> runProcess (cont msg)
runProcess (Fork p1 p2) = runProcess p1 >> runProcess p2 -- Sequential
runProcess (Done x)     = pure x

runConcurrent :: Process a -> IO a -- bad things happen here (no sync on putStrLn)
runConcurrent (Put msg p)  = putStrLn msg >> runConcurrent p
runConcurrent (Get cont)   = getLine >>= \msg -> runConcurrent (cont msg)
runConcurrent (Fork p1 p2) = forkIO (runConcurrent p1) >> runConcurrent p2
runConcurrent (Done x)     = pure x
-}

-- Examples

forkExample :: Process ()
forkExample = do
  fork $ do
    replicateM_ 3 (put "a")
    put "End of thread A"
  replicateM_ 3 (put "b")
  put "End of thread B"

--
