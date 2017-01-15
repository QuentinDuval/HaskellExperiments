module Arithmetic (

) where

import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set


type Id = String
type Env = Map.Map String Int
data OpType = Add | Mul deriving (Show, Eq, Ord)

data Expr
  = Cst Int
  | Var Id
  | Op OpType [Expr]
  deriving (Show, Eq, Ord)

-- Smart constructors

cst = Cst
var = Var
add = Op Add
mul = Op Mul

isCst (Cst _) = True
isCst _ = False


-- Evaluation (naive)

eval :: Env -> Expr -> Int
eval env (Cst n) = n
eval env (Var v) = env Map.! v
eval env (Op Add xs) = sum $ map (eval env) xs
eval env (Op Mul xs) = product $ map (eval env) xs

prn :: Expr -> String
prn (Cst n) = show n
prn (Var v) = v
prn (Op Add xs) = "(+ " ++ unwords (map prn xs) ++ ")"
prn (Op Mul xs) = "(* " ++ unwords (map prn xs) ++ ")"


-- Optimization

optimize :: Expr -> Expr
optimize op@(Op Add ys) = optOp Add (map optimize ys) 0 (+)
optimize op@(Op Mul ys)
  | not (null (dropWhile (/= cst 0) xs)) = cst 0
  | otherwise = optOp Mul xs 1 (*)
  where xs = map optimize ys
optimize e = e

optOp :: OpType -> [Expr] -> Int -> (Int -> Int -> Int) -> Expr
optOp opType xs neutral combine =
  let (constants, vars) = partition isCst xs
      constantsVal = map (\(Cst x) -> x) constants
      sumCst = foldl' combine neutral constantsVal
  in case vars of
      [] -> cst sumCst
      [y] | sumCst == neutral -> y
      ys -> Op opType (cst sumCst : ys)


-- Partial evals and dependencies

partial :: Env -> Expr -> Expr
partial env e@(Var v) =
  case Map.lookup v env of
    Nothing -> e
    Just n -> cst n
partial env (Op opType xs) = Op opType (map (partial env) xs) -- You have to care about recursion as well as transformations
partial env e = e

dependencies :: Expr -> Set.Set Id
dependencies (Var v) = Set.singleton v
dependencies (Op _ xs) = foldl1' Set.union (map dependencies xs)
dependencies e = Set.empty


-- Evaluation (alternate impl)

eval' :: Env -> Expr -> Int
eval' env e =
  case optimize (partial env e) of
    Cst n -> n
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
  -- Double tree traversal... more if you want to split up! Less incentive to do it due to performance hit.
  -- It is also pretty hard to test! You have to have the full tree!
  let f = optimize $ partial (Map.fromList [("y", 0)]) e
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
