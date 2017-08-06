module Money where

import qualified Data.Map as M

--

newtype Currency = Currency { currencyLabel :: String }
  deriving (Show, Eq, Ord)

data Money = Money { amount :: Double, currency :: Currency }
  deriving (Show, Eq, Ord)

data MoneyExpr
  = KnownAmount Money
  | MoneyAdd [MoneyExpr]
  | MoneyMul MoneyExpr Double
  deriving (Show, Eq, Ord)

--

money :: Double -> Currency -> MoneyExpr
money amount currency = KnownAmount $ Money amount currency

add :: MoneyExpr -> MoneyExpr -> MoneyExpr
add (KnownAmount m1) (KnownAmount m2)
  | currency m1 == currency m2 = money (amount m1 + amount m2) (currency m1)
add a b = MoneyAdd [a, b]

multiply :: MoneyExpr -> Double -> MoneyExpr
multiply (KnownAmount m) factor = KnownAmount $ m { amount = amount m * factor }
multiply expr factor = MoneyMul expr factor

evalMoneyIn :: (Market market) => market -> MoneyExpr -> Currency -> Maybe Money
evalMoneyIn market expr refCurrency = go expr
  where
    go (KnownAmount (Money amt curr)) = do
      rate <- conversionRate market (curr, refCurrency)
      pure $ Money (rate * amt) refCurrency

    go (MoneyAdd subs) = do
      ms <- mapM go subs
      pure $ Money (sum (map amount ms)) refCurrency

    go (MoneyMul expr factor) = do
      m <- go expr
      pure $ Money (factor * amount m) refCurrency


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
