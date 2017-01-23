module FreeMonad where

-- import Control.Monad.Free
import Control.Monad
import Data.List (tails)

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

-- FREE MONAD:
-- It is like the Fix point for the catamorphism but with Pure and r
-- It is only there to help you define your Monad... you do not need it

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

-- Our DSL

data Request cont
  = ReadLine (String -> cont)
  | WriteLine String cont

instance Functor Request where
  fmap f (ReadLine g)     = ReadLine (f . g)
  fmap f (WriteLine s g)  = WriteLine s (f g)

type Program = Free Request

interpret :: Program r -> IO r -- Going from one Monad to another
interpret (Free (ReadLine f)) = getLine >>= \l -> interpret (f l)
interpret (Free (WriteLine s f)) = putStrLn s >> interpret f

-- Helpers to build our language

liftF :: Functor f => f r -> Free f r
liftF x = Free (fmap Pure x) -- Wraps each leaf with Pure

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


--
