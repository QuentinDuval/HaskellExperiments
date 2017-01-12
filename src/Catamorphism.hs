{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE UndecidableInstances #-}
module Catamorphism (

) where

import qualified Data.Map as Map

-- All in Data.Fix

type Id = String
type Env = Map.Map String Int

data ExprR r
  = Cst Int
  | Var Id
  | Add [r]
  | Mul [r]
  deriving (
    Show, Eq, Ord,
    Functor, Foldable,
    Traversable)

newtype Fix f = Fix { unFix :: f (Fix f) } -- f = f f
type Expr = Fix ExprR -- Fix point of ExprR

instance Eq (f (Fix f)) => Eq (Fix f) where
  a == b = unFix a == unFix b

-- Smart constructors

cst = Fix . Cst
var = Fix . Var
add = Fix . Add
mul = Fix . Mul

-- Interpreters

cata :: (Functor f) => (f a -> a) -> Fix f -> a
cata algebra =
  algebra             -- Apply the algebra on top level
  .
  fmap (cata algebra) -- Apply the cata to the (Fix f)
  .
  unFix               -- Unwraps to get a f (Fix f)

eval :: Env -> Expr -> Int
eval env = cata alg where
  alg :: ExprR Int -> Int
  alg (Cst n) = n
  alg (Var x) = env Map.! x
  alg (Add xs) = sum xs
  alg (Mul xs) = product xs

prn :: Expr -> String
prn = cata alg where
  alg :: ExprR String -> String
  alg (Cst n) = show n
  alg (Var x) = x
  alg (Add xs) = "(+ " ++ unwords xs ++ ")"
  alg (Mul xs) = "(* " ++ unwords xs ++ ")"

-- Optimizers (to combine)

optAdd :: ExprR Expr -> Expr
optAdd (Add xs) =
  case filter (/= cst 0) xs of
      [] -> cst 0
      [y] -> y
      ys -> add ys
optAdd e = Fix e

optMul :: ExprR Expr -> Expr
optMul (Mul xs)
  | not (null (dropWhile (/= cst 0) xs)) = cst 0
  | otherwise
    = case filter (/= cst 1) xs of
        [] -> cst 0
        [y] -> y
        ys -> mul ys
optMul e = Fix e

-- TODO: Add fix that provides a variable

-- Optimizer

optimize :: Expr -> Expr
optimize = cata (optMul . unFix . optAdd)

-- Tests

testExpr :: IO ()
testExpr = do
  let env = Map.fromList [("x", 1), ("y", 2)]
  let e = add [ add [cst 1, cst 0, cst 0]
              , mul [cst 1, add [cst 2, var "y"]]
              , mul [cst 0, add [var "x", cst 1]]
              , var "x" ]
  let o = optimize e
  print $ prn e
  print $ prn o
  print $ eval env e
  print $ eval env o
  print $ "(+ 1 (+ 2 y) x)" == prn o

--
