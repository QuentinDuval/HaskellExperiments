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

data Term
  = Pos { getVar :: String }
  | Neg { getVar :: String }

newtype SAT = SAT { conjunctions ::  [[Term]]}

collectVars :: SAT -> [Var]
collectVars = Set.toList . Set.fromList . map getVar . concat . conjunctions

evalTerm :: Map.Map Var Bool -> Term -> Bool
evalTerm env term = env Map.! (getVar term)

evalSat :: Map.Map Var Bool -> SAT -> Bool
evalSat env = and . fmap (or . fmap (evalTerm env)) . conjunctions

sat :: SAT -> Bool
sat pb = or $ do
  let vars = collectVars pb
  vals <- replicate (length vars) [True, False]
  let env = Map.fromList (zip vars vals)
  pure $ evalSat env pb

test_sat :: IO ()
test_sat = do
  let formula = SAT [[Pos "x1", Pos "x2", Pos "x3"]
                    ,[Neg "x1", Neg "x4", Pos "x5"]]
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
