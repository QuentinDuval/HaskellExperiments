{-# LANGUAGE DeriveFunctor #-}

module MoneyCata where

import Data.Fix
import qualified Data.Map as M

--

newtype Currency = Currency { currencyLabel :: String }
  deriving (Show, Eq, Ord)

data Money = Money { amount :: Double, currency :: Currency }
  deriving (Show, Eq, Ord)

data MoneyExprR r
  = KnownAmount Money
  | MoneyAdd [r]
  | MoneyMul r Double
  deriving (Show, Eq, Ord, Functor)

type MoneyExpr = Fix MoneyExprR

--

money :: Double -> Currency -> MoneyExpr
money amount currency = Fix $ KnownAmount $ Money amount currency

add :: MoneyExpr -> MoneyExpr -> MoneyExpr
add a b = Fix $ MoneyAdd [a, b]

multiply :: MoneyExpr -> Double -> MoneyExpr
multiply expr factor = Fix $ MoneyMul expr factor

scale :: Money -> Double -> Money
scale (Money amt curr) factor = Money (amt * factor) curr

evalMoneyIn :: (Market market) => market -> MoneyExpr -> Currency -> Maybe Money
evalMoneyIn market expr refCurrency = cata go expr
  where
    go (KnownAmount m@Money{ currency = curr }) =
      scale m <$> conversionRate market (curr, refCurrency)
    go (MoneyAdd subs) = do
      ms <- sequence subs
      pure $ Money (sum (map amount ms)) refCurrency
    go (MoneyMul expr factor) =
      scale <$> expr <*> pure factor

class Market m where
  conversionRate :: m -> (Currency, Currency) -> Maybe Double

newtype FakeEnv = FakeEnv (M.Map (Currency, Currency) Double)
  deriving (Show, Eq, Ord)

instance Market FakeEnv where
  conversionRate (FakeEnv env) conversion@(from, to)
    | from == to = pure 1.0
    | otherwise  = M.lookup conversion env

--

test_money :: IO ()
test_money = do
  let a = money 30 (Currency "USD")
  let b = money 25 (Currency "EUR")
  let c = money 1000 (Currency "JPY")
  let x = add (add a (multiply b 2)) c
  print x
  let e = Money 100 (Currency "USD")
  let env = FakeEnv $ M.fromList
              [((Currency "EUR", Currency "USD"), 1.2)
              ,((Currency "JPY", Currency "USD"), 0.01)]
  print $ Just e == evalMoneyIn env x (Currency "USD")

--
