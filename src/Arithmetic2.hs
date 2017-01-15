{-# LANGUAGE RecordWildCards #-}
module Arithmetic2 (

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


-- Catamorphism

data ExprSem domain
  = ExprSem {
        onCst :: Int -> domain
      , onVar :: Id  -> domain
      , onAdd :: [domain] -> domain
      , onMul :: [domain] -> domain
    }

cata :: ExprSem domain -> Expr -> domain
cata ExprSem{..} = alg
  where
    alg (Cst n)     = onCst n
    alg (Var v)     = onVar v
    alg (Op Add xs) = onAdd (map alg xs)
    alg (Op Mul xs) = onMul (map alg xs)


comp :: ExprSem Expr -> ExprSem Expr -> ExprSem Expr
comp a b =
  ExprSem {
    onCst = visit a . onCst b
  , onVar = visit a . onVar b
  , onAdd = visit a . onAdd b
  , onMul = visit a . onMul b
  }
  where
    visit :: ExprSem Expr -> Expr -> Expr
    visit alg (Cst n) = onCst alg n
    visit alg (Var v) = onVar alg v
    visit alg (Op Add xs) = onAdd alg xs
    visit alg (Op Mul xs) = onMul alg xs


-- Smart constructors

cst = Cst
var = Var
add = Op Add
mul = Op Mul

isCst (Cst _) = True
isCst _ = False


-- Evaluation (cata)

eval :: Env -> Expr -> Int
eval env = cata (ExprSem id readInEnv sum product)
  where readInEnv v = env Map.! v

prn :: Expr -> String
prn = cata $ ExprSem {
                onCst = show
              , onVar = id
              , onAdd = \xs -> "(+ " ++ unwords xs ++ ")"
              , onMul = \xs -> "(* " ++ unwords xs ++ ")"
              }


-- Optimization

optimize_cata :: ExprSem Expr
optimize_cata =
  ExprSem {
    onCst = cst
  , onVar = var
  , onAdd = \xs -> optOp Add xs 0 (+)
  , onMul = \xs -> if not (null (dropWhile (/= cst 0) xs))
                      then cst 0
                      else optOp Mul xs 1 (*)
  }

optOp :: OpType -> [Expr] -> Int -> (Int -> Int -> Int) -> Expr
optOp opType xs neutral combine =
  let (constants, vars) = partition isCst xs
      constantsVal = map (\(Cst x) -> x) constants
      sumCst = foldl' combine neutral constantsVal
  in case vars of
      [] -> cst sumCst
      [y] | sumCst == neutral -> y
      ys -> Op opType (cst sumCst : ys)

optimize :: Expr -> Expr
optimize = cata optimize_cata


-- Partial evals and dependencies

partial_cata :: Env -> ExprSem Expr
partial_cata env =
  ExprSem {
    onCst = cst
  , onVar = \v -> case Map.lookup v env of
                    Nothing -> var v
                    Just nb -> cst nb
  , onAdd = add
  , onMul = mul
  }

partial :: Env -> Expr -> Expr
partial env = cata (optimize_cata `comp` partial_cata env)

dependencies :: Expr -> Set.Set Id
dependencies =
  cata $
    ExprSem {
      onCst = const Set.empty
    , onVar = Set.singleton
    , onAdd = foldl1' Set.union
    , onMul = foldl1' Set.union
    }


-- Evaluation (alternate impl)

eval' :: Env -> Expr -> Int
eval' env e =
  case cata (optimize_cata `comp` partial_cata env) e of
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
