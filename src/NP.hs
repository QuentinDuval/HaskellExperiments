module NP where

import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set

------------------------------------------------------------

headSafe :: [a] -> Maybe a
headSafe [] = Nothing
headSafe xs = Just (head xs)

------------------------------------------------------------

type Var = String
type Assignment = [(Var, Bool)]

data Term
  = Pos { getVar :: String }
  | Neg { getVar :: String }

newtype SAT = SAT { conjunctions ::  [[Term]]}

allVars :: SAT -> [Var]
allVars = Set.toList . Set.fromList . map getVar . concat . conjunctions

evalTerm :: Map.Map Var Bool -> Term -> Bool
evalTerm env (Pos var) = env Map.! var
evalTerm env (Neg var) = not (env Map.! var)

evalSat :: Assignment -> SAT -> Bool
evalSat assignment =
  let env = Map.fromList assignment
  in and . map (any (evalTerm env)) . conjunctions

sat :: SAT -> Maybe Assignment
sat pb = headSafe $ do
  assignment <- forM (allVars pb) $ \var -> do
    val <- [True, False]
    pure (var, val)
  guard (evalSat assignment pb)
  pure assignment

sat' :: SAT -> Bool
sat' pb = or $ do
  assignment <- forM (allVars pb) $ \var -> do
    val <- [True, False]
    pure (var, val)
  pure (evalSat assignment pb)

test_sat :: IO ()
test_sat = do
  let formula = SAT [[Pos "x1", Pos "x2", Pos "x3"]
                    ,[Neg "x1", Neg "x4", Neg "x5"]]
  print (sat formula)
  print (sat' formula)

------------------------------------------------------------

satExample :: Bool
satExample =
  or $ do
    x1 <- [True, False]
    x2 <- [True, False]
    -- ...
    xN <- [True, False]
    pure ((x1 || x2 || xN) && (not x1 || not x2 || xN))

satExample' :: Maybe [Bool]
satExample' =
  headSafe $ do
    x1 <- [True, False]
    x2 <- [True, False]
    -- ...
    xN <- [True, False]
    guard ((x1 || x2 || xN) && (not x1 || not x2 || xN))
    pure [x1, x2, xN]
