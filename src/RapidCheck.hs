{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RapidCheck where

import Control.Monad
import Data.Monoid((<>))
import System.Random


--------------------------------------------------------------------------------
-- Simplified version of a Quick Check library
--------------------------------------------------------------------------------

newtype Gen a = MkGen { runGen :: StdGen -> a } deriving (Functor)

newtype Property = MkProperty { asGenerator :: Gen Result }

data Result
  = Success
  | Failure { seed :: Int, failingInputs :: [String] }
  deriving (Show, Eq, Ord)

instance Monoid Result where
  mempty = Success
  mappend f@Failure{} _ = f
  mappend _ f@Failure{} = f
  mappend _ _ = Success

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
  MkProperty $ MkGen $ \stdGen ->
    let (stdGen1, stdGen2) = split stdGen
        x = runGen gen stdGen1
        pGen = asGenerator (property (prop x))
        r = runGen pGen stdGen2
    in case r of
      f@Failure{} -> f { failingInputs = show x : failingInputs f }
      Success -> Success

rapidCheck :: Testable prop => prop -> IO Result
rapidCheck = rapidCheckWith 100

rapidCheckWith :: Testable prop => Int -> prop -> IO Result
rapidCheckWith attemptNb prop = do
  seed <- randomIO
  return $ rapidCheckImpl attemptNb seed prop

replay :: Testable prop => Result -> prop -> Result
replay Success _ = Success
replay f@Failure{} prop = rapidCheckImpl 1 (seed f) prop

rapidCheckImpl :: Testable prop => Int -> Int -> prop -> Result
rapidCheckImpl attemptNb startSeed prop = runAll (asGenerator (property prop))
  where
    runAll gen = foldMap (runOne gen) [startSeed .. startSeed + attemptNb - 1]
    runOne gen seed =
      case runGen gen (mkStdGen seed) of
        Success -> Success
        f@Failure{} -> f { seed = seed }

{-
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
          b = runGen a gen1
      in runGen (f b) gen2

-- Alternative implementation based on the Applicative
runAttempts' :: Int -> Int -> Gen Result -> Result
runAttempts' attemptNb seed gen =
  let stdGen = mkStdGen seed
      gens = sequenceA (replicate attemptNb gen)
  in mconcat $ runGen gens stdGen
-}

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

instance Arbitrary Int where
  arbitrary = MkGen $ \gen -> fst (next gen)

instance CoArbitrary Int where
  coarbitrary n (MkGen g) = MkGen $ \gen -> g (variant n gen)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Fun a b) where
  arbitrary = promote (\a -> coarbitrary a arbitrary)

variant :: Int -> StdGen -> StdGen
variant n g = foldl (\g b -> side b (split g)) g (digits n)
  where side b = if b then snd else fst

digits :: Int -> [Bool]
digits =
  map ((== 0) . (`mod` 2))
  . takeWhile (> 0)
  . iterate (`div` 2)

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
  failure <- rapidCheck prop_addition_bad
  print failure
  print $ replay failure prop_addition_bad
  print =<< rapidCheck prop_composition


--
