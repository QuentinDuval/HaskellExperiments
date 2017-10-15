module PBTExamples where

import Data.List
import Data.Maybe

import qualified Data.Set as Set
import Data.Vector.Unboxed(Vector, (!))
import qualified Data.Vector.Unboxed as Vector

import Test.HUnit
import Test.QuickCheck


--------------------------------------------------------------------------------
-- First example
-- https://www.youtube.com/watch?v=XKu_SEDAykw
-- * The vector must be ordered
-- * Other cases (Hash map)
--------------------------------------------------------------------------------

pairSum :: Int -> Vector Int -> Maybe (Int, Int)
pairSum total ints = loop 0 (Vector.length ints - 1) where
  loop i j
    | i >= j = Nothing
    | otherwise =
      case compare (ints ! i + ints ! j) total of
        EQ -> Just (i, j)
        LT -> loop (succ i) j
        GT -> loop i (pred j)

hasPairSum :: Int -> Vector Int -> Bool
hasPairSum total = isJust . pairSum total

-- Unit tests

test_hasPairSum :: Test
test_hasPairSum = TestCase $ do
  assertEqual "Success case" True $ hasPairSum 8 (Vector.fromList [1, 3, 4, 4, 9])
  assertEqual "Success case" (Just (2, 3)) $ pairSum 8 (Vector.fromList [1, 3, 4, 4, 9])
  assertEqual "Failure case" False $ hasPairSum 8 (Vector.fromList [1, 3, 4, 6, 9])

all_tests :: IO Counts
all_tests = runTestTT test_hasPairSum

-- Proof:
-- INVARIANT = distance to the right position if they exist
-- It can only go down

-- Property based testing

prop_findExistingSum :: Int -> Int -> Property
prop_findExistingSum x y =
  forAll (listOf arbitrary) $ \ints ->
    hasPairSum (x + y) (Vector.fromList (sort (x : y : ints)))

prop_noExistingSum :: Property
prop_noExistingSum =
  forAll (listOf arbitrary) $ \ints ->
    let sums = Set.fromList [x + y | (x:ys) <- tails ints, y <- ys]
    in forAll arbitrary $ \total ->
      not (Set.member total sums) ==>
        not (hasPairSum total (Vector.fromList (sort ints)))

all_pbt_tests :: IO ()
all_pbt_tests = do
  quickCheck prop_findExistingSum
  quickCheck prop_noExistingSum

--
