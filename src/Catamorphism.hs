{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE UndecidableInstances #-}
module Catamorphism (

) where

import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

-- All in Data.Fix

type Id = String
type Env = Map.Map String Int
data OpType = Add | Mul deriving (Show, Eq, Ord)

data ExprR r
  = Cst Int
  | Var Id
  | Op OpType [r]
  deriving (Show, Eq, Ord)

newtype Fix f = Fix { unFix :: f (Fix f) } -- f = f f
type Expr = Fix ExprR -- Fix point of ExprR

instance Eq (f (Fix f)) => Eq (Fix f) where
  a == b = unFix a == unFix b

instance Functor ExprR where
  fmap _ (Cst c) = Cst c
  fmap _ (Var v) = Var v
  fmap f (Op opType xs) = Op opType (map f xs)

-- Smart constructors

cst = Fix . Cst
var = Fix . Var
add = Fix . Op Add
mul = Fix . Op Mul

isCst (Fix (Cst _)) = True
isCst _ = False

-- Catamorphism

cata :: (Functor f) => (f a -> a) -> Fix f -> a
cata algebra =
  algebra             -- Apply the algebra on top level
  .
  fmap (cata algebra) -- Apply the cata to the (Fix f)
  .
  unFix               -- Unwraps to get a f (Fix f)

-- Composition

type FixAlg f = f (Fix f) -> Fix f

comp :: FixAlg f -> FixAlg f -> FixAlg f
comp f g = f . unFix . g

compAll :: Foldable t => t (FixAlg f) -> FixAlg f
compAll fs = foldr1 comp fs

-- Interpreters

eval :: Env -> Expr -> Int
eval env = cata alg where
  alg (Cst n) = n
  alg (Var x) = env Map.! x
  alg (Op Add xs) = sum xs
  alg (Op Mul xs) = product xs

prn :: Expr -> String
prn = cata alg where
  alg (Cst n) = show n
  alg (Var x) = x
  alg (Op Add xs) = "(+ " ++ unwords xs ++ ")"
  alg (Op Mul xs) = "(* " ++ unwords xs ++ ")"

-- Optimizers (to combine)

optOp :: ExprR Expr -> Int -> (Int -> Int -> Int) -> Expr
optOp (Op optType xs) neutral combine =
  let (constants, vars) = partition isCst xs
      constantsVal = map (\(Fix (Cst x)) -> x) constants
      sumCst = foldl' combine neutral constantsVal
  in case vars of
      [] -> cst sumCst
      [y] | sumCst == neutral -> y
      ys -> Fix $ Op optType (cst sumCst : ys)

optAdd :: ExprR Expr -> Expr
optAdd op@(Op Add _) = optOp op 0 (+)
optAdd e = Fix e

optMul :: ExprR Expr -> Expr
optMul op@(Op Mul xs)
  | not (null (dropWhile (/= cst 0) xs)) = cst 0
  | otherwise = optOp op 1 (*)
optMul e = Fix e

-- Optimizer

optimize :: Expr -> Expr
optimize = cata (optMul `comp` optAdd)

-- Partial evals

replaceVar :: Env -> ExprR Expr -> Expr
replaceVar env = go where
  go e@(Var v) =
    case Map.lookup v env of
      Just val -> cst val
      Nothing -> Fix e
  go e = Fix e

partial :: Env -> Expr -> Expr
partial env = cata (compAll [optMul, optAdd, replaceVar env])

-- Collector of variables

dependencies :: Expr -> Set.Set Id
dependencies = cata alg where
  alg (Cst _) = Set.empty
  alg (Var x) = Set.singleton x
  alg (Op _ xs) = foldl1' Set.union xs

-- Eval 2.0 (based on the fixing)

eval' :: Env -> Expr -> Int
eval' env expr =
  case partial env expr of
    (Fix (Cst n)) -> n
    e -> error $ "Missing vars: " ++ show (dependencies e)

-- Tests

testExpr :: IO ()
testExpr = do
  let env = Map.fromList [("x", 1), ("y", 2)]
  let e = add [ cst(1)
              , cst(2)
              , mul [cst(0), var("x"), var("y")]
              , mul [cst(1), var("y"), cst(2)]
              , add [cst(0), var("x") ]
              ]
  let o = optimize e
  let f = partial (Map.fromList [("y", 0)]) e
  print $ prn e
  print $ prn o
  print $ prn f
  print $ dependencies o
  print $ dependencies f
  print $ eval env e
  print $ eval env o
  print $ eval' env e
  print $ eval' env o
  print $ "(+ 3 (* 2 y) x)" == prn o

--
