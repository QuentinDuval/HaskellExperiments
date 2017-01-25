{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module FreeMonad where

-- import Control.Monad.Free
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Reader
import Data.List (tails)
import qualified Data.Map as Map
import System.IO

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
-- Our DSL 1
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
-- Our DSL 3
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

runProcess :: Process a -> IO a  -- TODO: too much similarities: catamorphism?
runProcess (Free (Put msg p))  = putStrLn msg >> runProcess p
runProcess (Free (Get cont))   = getLine >>= \msg -> runProcess (cont msg)
runProcess (Free (Fork p1 p2)) = runProcess p1 >> runProcess p2 -- Sequential
runProcess (Pure x)            = pure x

------------

data Msg
  = Send String
  | Stop (MVar ())

runConcurrent :: Process a -> IO a -- bad things happen here (no sync on putStrLn)
runConcurrent p = do
  c <- newChan
  executor <- forkIO (loop c)
  r <- runWith c p
  v <- newEmptyMVar
  writeChan c (Stop v)
  takeMVar v
  return r
  where
    loop c = do
      str <- readChan c
      case str of
        Send str -> print str >> loop c
        Stop var -> putMVar var ()

    runWith :: Chan Msg -> Process b -> IO b
    runWith c (Free (Put msg p))  = writeChan c (Send msg) >> yield >> runWith c p
    runWith c (Free (Get cont))   = getLine >>= \msg -> runWith c (cont msg)
    runWith c (Free (Fork p1 p2)) = forkIO (runWith c p1) >> runWith c p2
    runWith c (Pure x)            = pure x

--

runMultiProcess :: [Process ()] -> IO () -- Does not really work with any output => we need a main thrad
runMultiProcess (Free (Put msg p) : ps)  = putStrLn msg >> runMultiProcess (ps ++ [p])
runMultiProcess (Free (Get cont) : ps)   = getLine >>= \msg -> runMultiProcess (ps ++ [cont msg])
runMultiProcess (Free (Fork p1 p2) : ps) = runMultiProcess (p1 : p2 : ps)
runMultiProcess (Pure _ : ps)     = runMultiProcess ps
runMultiProcess []                = pure ()



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
    forM_ ['a'..'z'] $ \c -> put [c]
    put "End of Letters"
  forM_ [0..26] $ \i -> put (show i)
  put "End of Numbers"


--------------------------------------------------------------------------------
-- Our DSL 4: PrintF
--------------------------------------------------------------------------------

foo :: FooType a => a
foo = bar (return ())

class FooType a where
    bar :: IO () -> a

instance FooType (IO ()) where
    bar = id

instance (Show x, FooType r) => FooType (x -> r) where
    bar s = \x -> bar (s >> print x)

testFoo :: IO ()
testFoo = foo 3 4 5 "abc"


--------------------------------------------------------------------------------
-- Our DSL 5: Like quick check
--------------------------------------------------------------------------------

class Sample a where
  sample :: a

class Testable a where
  evalPred :: a -> Bool

instance Testable Bool where
  evalPred b = b

instance (Sample x, Testable r) => Testable (x -> r) where
  evalPred p = evalPred (p sample)

instance Sample Int where
  sample = 0

testableEx :: Int -> Int -> Bool
testableEx a b = a == b

testSample :: IO ()
testSample = print $ evalPred testableEx

--
