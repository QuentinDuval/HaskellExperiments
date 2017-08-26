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
type Assignment = Map.Map Var Bool

data Term
  = Pos { getVar :: String }
  | Neg { getVar :: String }

newtype SAT = SAT { conjunctions ::  [[Term]]}

collectVars :: SAT -> [Var]
collectVars = Set.toList . Set.fromList . map getVar . concat . conjunctions

evalTerm :: Assignment -> Term -> Bool
evalTerm env (Pos var) = env Map.! var
evalTerm env (Neg var) = not (env Map.! var)

evalSat :: Assignment -> SAT -> Bool
evalSat env = and . map (any (evalTerm env)) . conjunctions

sat :: SAT -> Maybe Assignment
sat pb = headSafe $ do
  guesses <- forM (collectVars pb) $ \var -> do
    val <- [True, False]
    pure (var, val)
  let assignment = Map.fromList guesses
  guard (evalSat assignment pb)
  pure assignment

test_sat :: IO ()
test_sat = do
  let formula = SAT [[Pos "x1", Pos "x2", Pos "x3"]
                    ,[Neg "x1", Neg "x4", Neg "x5"]]
  print (sat formula)

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
