{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RapidCheck where

import Control.Monad
import System.Random


--------------------------------------------------------------------------------
-- Simplified version of a Quick Check library
--------------------------------------------------------------------------------

newtype Gen a = MkGen { runGen :: StdGen -> a }
  deriving (Functor)

instance Applicative Gen where
  pure a = MkGen (const a)
  f <*> a =
    MkGen $ \gen ->
      let (gen1, gen2) = split gen
      in (runGen f gen1) (runGen a gen2)

instance Monad Gen where
  a >>= f =
    MkGen $ \gen ->
      let (gen1, gen2) = split gen
          b = f (runGen a gen1)
      in runGen b gen2

newtype Property = MkProperty { asGenerator :: Gen Result }

data Result
  = Success
  | Failure { seed :: Int, failingInputs :: [String] }
  deriving (Show, Eq, Ord)

instance Monoid Result where
  mempty = Success
  mappend f@Failure{} _ = f
  mappend _ f@Failure{} = f
  mappend Success Success = Success

--------------------------------------------------------------------------------

class Arbitrary a where
  arbitrary :: Gen a

class CoArbitrary a where
  coarbitrary :: a -> Gen b -> Gen b

class Testable a where
  property :: a -> Property

instance Testable Result where
  property r = MkProperty $ MkGen $ return r

instance Testable Bool where
  property = property . toResult where
    toResult b = if b then Success else Failure 0 []

instance Testable Property where
  property = id

instance (Show a, Arbitrary a, Testable prop) => Testable (a -> prop) where
  property f = forAll arbitrary f


-- | For all values generated by `Gen a`, evaluate the property with the generated a

forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
forAll gen prop =
  MkProperty $ do
    x <- gen -- TODO: require Gen as monad
    r <- asGenerator (property (prop x))
    case r of
      f@Failure{} -> return $ f { failingInputs = show x : failingInputs f }
      Success -> return Success

rapidCheck :: Testable prop => prop -> IO Result
rapidCheck prop = do
  seed <- randomIO
  let result = rapidCheckWith 100 seed prop
  return result

rapidCheckWith :: Testable prop => Int -> Int -> prop -> Result
rapidCheckWith attemptNb seed prop =
  let stdGen = mkStdGen seed
      gen = asGenerator (property prop)
      gens = sequenceA (replicate attemptNb gen) -- TODO: requires Gen as Applicative
      result = mconcat $ runGen gens stdGen
  in case result of
    Success -> Success
    f@Failure{} -> f { seed = seed }


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

instance Arbitrary Int where
  arbitrary = MkGen $ \gen -> fst (next gen)

instance CoArbitrary Int where
  coarbitrary _ g = g -- TODO: really bad implementation

instance (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b) where
  arbitrary = fmap apply arbitrary

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Fun a b) where
  arbitrary = promote (\a -> coarbitrary a arbitrary)

promote :: (a -> Gen b) -> Gen (Fun a b)
promote f = MkGen $ \gen -> Fun $ \a -> let g = f a in runGen g gen

data Fun a b = Fun { apply :: a -> b }
instance Show (Fun a b) where show _ = "<function>"

--------------------------------------------------------------------------------
-- Example
--------------------------------------------------------------------------------

prop_addition :: Int -> Int -> Bool
prop_addition a b = a + b == b + a

prop_addition_bad :: Int -> Int -> Bool
prop_addition_bad a b = a + a == b + b

prop_composition :: Int -> Fun Int Int -> Fun Int Int -> Bool
prop_composition i (Fun f) (Fun g) = f (g i) == g (f i)

runTests :: IO ()
runTests = do
  print =<< rapidCheck prop_addition
  print =<< rapidCheck prop_addition_bad
  print =<< rapidCheck prop_composition


--
