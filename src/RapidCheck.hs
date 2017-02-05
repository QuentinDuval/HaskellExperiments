{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module RapidCheck where

import Control.Monad
import Data.List
import Data.Monoid((<>))
import System.Random
import Text.Show.Functions


--------------------------------------------------------------------------------
-- Result type
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
  mappend _ rhs = rhs

overFailure :: Result -> (Result -> Result) -> Result
overFailure Success _ = Success
overFailure failure f = f failure

isSuccess :: Result -> Bool
isSuccess Success = True
isSuccess _ = False

addToCounterExample :: (Show a) => a -> Result -> Result
addToCounterExample arg failure =
  failure { counterExample = show arg : counterExample failure }


--------------------------------------------------------------------------------
-- Generators and properties
--------------------------------------------------------------------------------

newtype Gen a = Gen { runGen :: StdGen -> a }

newtype Property = Property { getGen :: Gen Result }

runProp :: Property -> StdGen -> Result
runProp prop rand = runGen (getGen prop) rand


--------------------------------------------------------------------------------
-- Main type classes: Arbitrary, CoArbitrary and Testable
--------------------------------------------------------------------------------

class Arbitrary a where
  arbitrary :: Gen a
  shrink :: a -> [a]
  shrink _ = []

class CoArbitrary a where
  coarbitrary :: Gen b -> a -> Gen b

class Testable a where
  property :: a -> Property


--------------------------------------------------------------------------------
-- Induction on Testable to support function with arbitrary number of arguments
--------------------------------------------------------------------------------

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
  property = forAll arbitrary shrink


--------------------------------------------------------------------------------
-- forAll, the heart of property based testing
--------------------------------------------------------------------------------

forAll :: (Show a, Testable testable) => Gen a -> (a -> [a]) -> (a -> testable) -> Property
forAll argGen argShrink prop =
  Property $ Gen $ \rand ->             -- Create a new property that will
    let (rand1, rand2) = split rand     -- Split the generator in two
        arg = runGen argGen rand1       -- Use the first generator to produce an arg
        subProp = property (prop arg)   -- Use the `a` to access the sub-property
        result = runProp subProp rand2  -- Use the second generator to run it
    in overFailure result $ \failure ->     -- Handle the failure case:
        shrinking argShrink arg prop rand2  -- Attempt to shrink the counter example
        <> addToCounterExample arg failure  -- In case the shrining failed


--------------------------------------------------------------------------------
-- Shrinking process
--------------------------------------------------------------------------------

shrinking :: (Show a, Testable testable) => (a -> [a]) -> a -> (a -> testable) -> StdGen -> Result
shrinking shrink arg prop rand =
  let smaller = treePostWalk arg shrink
      results = map (\a -> (a, runProp (property (prop a)) rand)) smaller
      failures = dropWhile (isSuccess . snd) results
  in case failures of
      [] -> Success
      ((reduced, failure):_) -> addToCounterExample reduced failure

treePostWalk :: a -> (a -> [a]) -> [a]
treePostWalk root leaves = visit [root] where
  visit [] = []
  visit xs = concatMap (visit . leaves) xs ++ xs


--------------------------------------------------------------------------------
-- rapidCheck, our main entry point
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


--------------------------------------------------------------------------------
-- TESTS: Instances
--------------------------------------------------------------------------------

instance Arbitrary Int where
  arbitrary = Gen $ \rand -> fst (next rand)
  shrink = shrinkIntegral

instance Arbitrary Integer where
  arbitrary = Gen $ \rand -> fromIntegral $ fst (next rand)
  shrink = shrinkIntegral

shrinkIntegral :: (Integral n) => n -> [n]
shrinkIntegral n
  | n == 0 = []
  | otherwise =
    [abs n | n < 0]
    ++ 0 : takeWhile (\m -> abs m < abs n)
            [ n - i | i <- tail (iterate (`quot` 2) n)]

instance Arbitrary Bool where
  arbitrary = Gen $ \rand -> odd (fst (next rand))

instance (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b) where
  arbitrary = promote (coarbitrary arbitrary)

promote :: (a -> Gen b) -> Gen (a -> b)
promote f = Gen $ \rand a -> runGen (f a) rand

instance Arbitrary a => Arbitrary [a] where
  arbitrary =
    Gen $ \rand ->
      let (rand1, rand2) = split rand
          len = fst (randomR (0,10) rand1)
          rands = take len (variants rand2)
      in map (runGen arbitrary) rands

instance CoArbitrary Integer where
  coarbitrary gen n = Gen $ \rand -> runGen gen (perturb n rand)

instance CoArbitrary [Int] where
  coarbitrary gen xs =
    Gen $ \rand ->
      runGen gen (foldr perturb (perturb 0 rand) xs)

perturb :: (Integral n) => n -> StdGen -> StdGen
perturb n rand0 =
  foldl
    (\rand b -> vary b rand)  -- Vary generator based on digit value
    (vary (n < 0) rand0)      -- Vary generator based on sign
    (digits (abs n))          -- Decompose a positive number in digits
  where
    vary digit rand =
      (if digit then snd else fst)
      (split rand)
    digits =
      map ((== 0) . (`mod` 2))
      . takeWhile (> 0)
      . iterate (`quot` 2)

variants :: StdGen -> [StdGen]
variants rand = rand1 : variants rand2
  where (rand1, rand2) = split rand


--------------------------------------------------------------------------------
-- TEST: run them with runTests
--------------------------------------------------------------------------------

prop_stupid :: Integer -> Integer -> Bool
prop_stupid a b = a == b

prop_gcd :: Integer -> Integer -> Bool
prop_gcd a b = a * b == gcd a b * lcm a b

prop_gcd_bad :: Integer -> Integer -> Bool
prop_gcd_bad a b = gcd a b > 1

prop_gcd_overflow :: Int -> Int -> Bool
prop_gcd_overflow a b = a * b == gcd a b * lcm a b

prop_partition :: [Integer] -> (Integer -> Bool) -> Bool
prop_partition xs p =
  let (lhs, rhs) = partition p xs
  in and
      [ all p lhs
      , not (any p rhs)
      , sort xs == sort (lhs ++ rhs) ]

prop_partition_2 :: [[Int]] -> ([Int] -> Bool) -> Bool
prop_partition_2 xs p =
  let (lhs, rhs) = partition p xs
  in and
      [ all p lhs
      , not (any p rhs)
      , sort xs == sort (lhs ++ rhs) ]

prop_distributive :: Integer -> Integer -> (Integer -> Integer) -> Bool
prop_distributive a b f = f (a + b) == f a + f b

runTests :: IO ()
runTests = do
  print =<< rapidCheck prop_stupid
  print =<< rapidCheck prop_gcd
  print =<< rapidCheck prop_gcd_overflow
  failure <- rapidCheck prop_gcd_bad
  print failure
  print $ replay failure prop_gcd_bad
  print "Higher order functions"
  print =<< rapidCheck prop_partition
  print =<< rapidCheck prop_partition_2
  print =<< rapidCheck prop_distributive

--
