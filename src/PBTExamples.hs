module PBTExamples where

import Data.Vector.Unboxed(Vector)
import Data.Vector.Unboxed as Vector

import Test.HUnit
import Test.QuickCheck


--------------------------------------------------------------------------------
-- First example
-- https://www.youtube.com/watch?v=XKu_SEDAykw
--------------------------------------------------------------------------------

hasPairSum :: Int -> Vector Int -> Bool
hasPairSum total = loop where
  loop ints =
    if Vector.length ints < 2 then False else
      let currentSum = Vector.head ints + Vector.last ints
      in case compare currentSum total of
        EQ -> True
        LT -> loop (Vector.tail ints)
        GT -> loop (Vector.init ints)

test_hasPairSum :: Test
test_hasPairSum = TestCase $ do
  assertEqual "Success case" True $ hasPairSum 8 (Vector.fromList [1, 3, 4, 4, 9])
  assertEqual "Failure case" False $ hasPairSum 8 (Vector.fromList [1, 3, 4, 6, 9])

all_tests :: IO Counts
all_tests = runTestTT test_hasPairSum

--
