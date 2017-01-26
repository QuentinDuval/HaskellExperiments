{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RapidCheck where

import System.Random


--------------------------------------------------------------------------------
-- Simplified version
--------------------------------------------------------------------------------

newtype Gen a    = MkGen { unGen :: IO a } deriving (Functor, Applicative, Monad)
newtype Property = MkProperty { unProperty :: Gen Bool }

class Arbitrary a where
  arbitrary :: Gen a

class Testable a where
  property :: a -> Property

instance Testable Bool where
  property b = MkProperty $ MkGen $ return b

instance Testable Property where
  property = id

instance (Show a, Arbitrary a, Testable prop) => Testable (a -> prop) where
  property f = forAll arbitrary f


-- | For all values generated by `Gen a`, evaluate the property with the generated a

forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
forAll gen prop =
  MkProperty $ do
    x <- gen
    unProperty (property (prop x))

rapidCheck :: Testable prop => prop -> IO Bool
rapidCheck prop = unGen (unProperty (property prop)) -- Run a single test (TODO - run several time this expression)


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

instance Arbitrary Int where
  arbitrary = MkGen randomIO


--------------------------------------------------------------------------------
-- Example
--------------------------------------------------------------------------------

prop_addition :: Int -> Int -> Bool
prop_addition a b = a + b == b + a

prop_addition_bad :: Int -> Int -> Bool
prop_addition_bad a b = a + a == b + b

runTests :: IO ()
runTests = do
  print =<< rapidCheck prop_addition
  print =<< rapidCheck prop_addition_bad


--
