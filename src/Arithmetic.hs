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

optimize :: Expr -> Expr
optimize e = e

partial :: Env -> Expr -> Expr
partial env e = e

dependencies :: Expr -> Set.Set Id
dependencies e = Set.empty


-- Evaluation (natural fold)

eval' :: Env -> Expr -> Int
eval' env e = eval env e


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
