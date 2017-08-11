module Money2 where

import qualified Data.Map as M

--

type Amount = Double
type Rate = Double
type Conversion = (Currency, Currency)

newtype Currency = Currency { currencyLabel :: String }
  deriving (Show, Eq, Ord)

data Money = Money { amount :: Amount, currency :: Currency }
  deriving (Show, Eq, Ord)

data MoneyBag = MoneyBag (M.Map Currency Amount)
  deriving (Show, Eq, Ord)

--

money :: Amount -> Currency -> MoneyBag
money amount currency = MoneyBag $ M.fromList [(currency, amount)]

add :: MoneyBag -> MoneyBag -> MoneyBag
add (MoneyBag m1) (MoneyBag m2) = MoneyBag $ M.unionWith (+) m1 m2

multiply :: MoneyBag -> Double -> MoneyBag
multiply (MoneyBag m) f = MoneyBag $ fmap (* f) m

evalMoneyIn :: (Conversion -> Maybe Rate) -> MoneyBag -> Currency -> Maybe Money
evalMoneyIn conversionRate (MoneyBag m) refCurrency = do
  let convert (curr, amount) = (* amount) <$> conversionRate (curr, refCurrency)
  amounts <- mapM convert (M.toList m)
  pure $ Money { amount = sum amounts, currency = refCurrency }

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
  print $ x == add a (add (multiply b 2) c)

--
