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

comp f g = f . unFix . g
comps fs = undefined -- TODO

-- Interpreters

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
  let (constants, vars) = partition isCst xs
      sumCst = sum $ map (\(Fix (Cst x)) -> x) constants
  in case vars of
      []  -> cst sumCst
      [y] | sumCst == 0 -> y
      ys -> add (cst sumCst : ys)
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

-- Optimizer

optimize :: Expr -> Expr
optimize = cata (optMul `comp` optAdd)

-- Fixings

fixVar :: Env -> ExprR Expr -> Expr
fixVar env = go where
  go e@(Var v) =
    case Map.lookup v env of
      Just val -> cst val
      Nothing -> Fix e
  go e = Fix e

fixing :: Env -> Expr -> Expr
fixing env = cata (optMul `comp` optAdd `comp` fixVar env)

-- Collector of variables

dependencies :: Expr -> Set.Set Id
dependencies = cata alg where
  alg :: ExprR (Set.Set Id) -> Set.Set Id
  alg (Cst _) = Set.empty
  alg (Var x) = Set.singleton x
  alg (Add xs) = foldl1' Set.union xs
  alg (Mul xs) = foldl1' Set.union xs

-- Eval 2.0 (based on the fixing)

eval' :: Env -> Expr -> Int
eval' env expr =
  case fixing env expr of
    (Fix (Cst n)) -> n
    e -> error $ "Missing vars: " ++ show (dependencies e)

-- Tests

testExpr :: IO ()
testExpr = do
  let env = Map.fromList [("x", 1), ("y", 2)]
  let e = add [ add [cst 1, cst 0, cst 0]
              , mul [cst 1, add [cst 2, var "y"]]
              , mul [cst 0, add [var "x", cst 1]]
              , var "x" ]
  let o = optimize e
  let f = fixing (Map.fromList [("y", 0)]) e
  print $ prn e
  print $ prn o
  print $ prn f
  print $ dependencies o
  print $ dependencies f
  print $ eval env e
  print $ eval env o
  print $ eval' env e
  print $ eval' env o
  print $ "(+ 1 (+ 2 y) x)" == prn o

--
