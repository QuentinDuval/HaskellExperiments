module Money where

import qualified Data.Map as M

--

type Amount = Double
type Rate = Double
type Conversion = (Currency, Currency)

newtype Currency = Currency { currencyLabel :: String }
  deriving (Show, Eq, Ord)

data Money = Money { amount :: Amount, currency :: Currency }
  deriving (Show, Eq, Ord)

data MoneyExpr
  = KnownAmount Money
  | MoneyAdd [MoneyExpr]
  | MoneyMul MoneyExpr Double
  deriving (Show, Eq) -- TODO: problem Eq means nothing  

--

money :: Amount -> Currency -> MoneyExpr
money amount currency = KnownAmount $ Money amount currency

add :: MoneyExpr -> MoneyExpr -> MoneyExpr
add (KnownAmount m1) (KnownAmount m2)
  | currency m1 == currency m2 = money (amount m1 + amount m2) (currency m1)
add a b = MoneyAdd [a, b]

multiply :: MoneyExpr -> Double -> MoneyExpr
multiply (KnownAmount m) factor = KnownAmount $ m { amount = amount m * factor }
multiply expr factor = MoneyMul expr factor

evalMoneyIn :: (Conversion -> Maybe Rate) -> MoneyExpr -> Currency -> Maybe Money
evalMoneyIn conversionRate expr refCurrency = go expr
  where
    go (KnownAmount (Money amt curr)) = do
      rate <- conversionRate (curr, refCurrency)
      pure $ Money (rate * amt) refCurrency

    go (MoneyAdd subs) = do
      ms <- mapM go subs
      pure $ Money (sum (map amount ms)) refCurrency

    go (MoneyMul expr factor) = do
      m <- go expr
      pure $ Money (factor * amount m) refCurrency

conversionRate :: M.Map Conversion Rate -> Conversion -> Maybe Rate
conversionRate fakeMarket conversion@(from, to)
  | from == to = pure 1.0
  | otherwise  = M.lookup conversion fakeMarket

--

test_money :: IO ()
test_money = do
  let a = money 30 (Currency "USD")
  let b = money 25 (Currency "EUR")
  let c = money 1000 (Currency "JPY")
  let x = add (add a (multiply b 2)) c
  print x
  putStrLn (replicate 10 '-')

  let e = Money 100 (Currency "USD")
  let rates = conversionRate $ M.fromList
              [((Currency "EUR", Currency "USD"), 1.2)
              ,((Currency "JPY", Currency "USD"), 0.01)]

  print $ Just e == evalMoneyIn rates x (Currency "USD")
  print $ x == add a (add (multiply b 2) c) -- broken by design

--
