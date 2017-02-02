{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RapidCheck where

import Control.Monad
import Data.Monoid((<>))
import System.Random


--------------------------------------------------------------------------------
-- Simplified version of a Quick Check library
--------------------------------------------------------------------------------

data Result
  = Success
  | Failure {
    seed :: Int,
    counterExample :: [String]
  } deriving (Show, Eq, Ord)

instance Monoid Result where
  mempty = Success
  mappend f@Failure{} _ = f
  mappend _ f@Failure{} = f
  mappend _ _ = Success

overFailure :: Result -> (Result -> Result) -> Result
overFailure Success _ = Success
overFailure failure f = f failure

newtype Gen a = Gen { runGen :: StdGen -> a }

newtype Property = Property { getGen :: Gen Result }

runProp :: Property -> StdGen -> Result
runProp prop rand = runGen (getGen prop) rand

--------------------------------------------------------------------------------

class Arbitrary a where
  arbitrary :: Gen a

class CoArbitrary a where
  coarbitrary :: a -> Gen b -> Gen b

class Testable a where
  property :: a -> Property

instance Testable Property where
  property = id

instance Testable Result where
  property r = Property (Gen (const r))

instance Testable Bool where
  property = property . toResult where
      toResult b = if b then Success
                        else Failure { seed = 0, counterExample = []}

instance (Show a, Arbitrary a, Testable testable)
         => Testable (a -> testable) where
  property = forAll arbitrary


-- | For all values generated by `Gen a`, evaluate the property with the generated a

forAll :: (Show a, Testable testable) => Gen a -> (a -> testable) -> Property
forAll argGen prop =
  Property $ Gen $ \rand ->             -- Create a new property that will
    let (rand1, rand2) = split rand     -- Split the generator in two
        arg = runGen argGen rand1       -- Use the first generator to produce an arg
        subProp = property (prop arg)   -- Use the `a` to access the sub-property
        result = runProp subProp rand2  -- Use the second generator to run it
    in overFailure result $ \failure -> -- Enrich the result with the argument
        failure { counterExample = show arg : counterExample failure }

--------------------------------------------------------------------------------

rapidCheck :: Testable prop => prop -> IO Result
rapidCheck = rapidCheckWith 100

rapidCheckWith :: Testable prop => Int -> prop -> IO Result
rapidCheckWith attemptNb prop = do
  seed <- randomIO
  return $ rapidCheckImpl attemptNb seed prop

replay :: Testable prop => Result -> prop -> Result
replay result prop =
  overFailure result $ \failure -> rapidCheckImpl 1 (seed failure) prop

rapidCheckImpl :: Testable prop => Int -> Int -> prop -> Result
rapidCheckImpl attemptNb startSeed prop = runAll (property prop)
  where
    runAll prop = foldMap (runOne prop) [startSeed .. startSeed + attemptNb - 1]
    runOne prop seed =
      let result = runProp prop (mkStdGen seed)
      in overFailure result $ \failure -> failure { seed = seed }

{-
instance Applicative Gen where
  pure a = Gen (const a)
  f <*> a =
    Gen $ \gen ->
      let (gen1, gen2) = split gen
      in (runGen f gen1) (runGen a gen2)

instance Monad Gen where
  a >>= f =
    Gen $ \gen ->
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
  arbitrary = Gen $ \rand -> fst (next rand)

instance Arbitrary Integer where
  arbitrary = Gen $ \rand -> fromIntegral $ fst (next rand)

instance CoArbitrary Integer where
  coarbitrary n (Gen g) = Gen $ \rand -> g (variant n rand)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Fun a b) where
  arbitrary = promote (\a -> coarbitrary a arbitrary)

variant :: (Integral n) => n -> StdGen -> StdGen
variant n randGen0 =
  foldl
    (\randGen b -> side b (split randGen))
    (side (n > 0) (split randGen0))
    (digits (abs n))
  where
    side b = if b then snd else fst
    digits =
      map ((== 0) . (`mod` 2))
      . takeWhile (> 0)
      . iterate (`div` 2)

promote :: (a -> Gen b) -> Gen (Fun a b)
promote f = Gen $ \gen -> Fun $ \a -> let g = f a in runGen g gen

data Fun a b = Fun { apply :: a -> b }
instance Show (Fun a b) where show _ = "<function>"

--------------------------------------------------------------------------------
-- Example
--------------------------------------------------------------------------------

prop_gcd :: Integer -> Integer -> Bool
prop_gcd a b = a * b == gcd a b * lcm a b

prop_gcd_bad :: Integer -> Integer -> Bool
prop_gcd_bad a b = gcd a b > 1

prop_gcd_overflow :: Int -> Int -> Bool
prop_gcd_overflow a b = a * b == gcd a b * lcm a b

prop_composition :: Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool
prop_composition i (Fun f) (Fun g) = f (g i) == g (f i)

runTests :: IO ()
runTests = do
  print =<< rapidCheck prop_gcd
  print =<< rapidCheck prop_gcd_overflow
  failure <- rapidCheck prop_gcd_bad
  print failure
  print $ replay failure prop_gcd_bad
  print =<< rapidCheck prop_composition

--
