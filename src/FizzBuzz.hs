module FizzBuzz where

import Data.List
import Test.QuickCheck
import Test.QuickCheck.Function


-- IMPL

fizzBuzz :: Int -> String
fizzBuzz = fizzBuzzImpl [(3, "Fizz"), (5, "Buzz")]

fizzBuzzImpl :: [(Int, String)] -> Int -> String
fizzBuzzImpl rules n = concatMap snd $ filter (divisible n . fst) rules

divisible :: Int -> Int -> Bool
divisible n d = rem n d == 0

-- TESTS

prop_fizzBuzz :: Int -> Int -> Property
prop_fizzBuzz a b =
  fizzBuzz a == "Fizz" && fizzBuzz b == "Buzz"
  ==> fizzBuzz (a * b) == "FizzBuzz"

prop_fizzBuzz2 :: Property
prop_fizzBuzz2 =
  forAll (arbitrary `suchThat` \i -> fizzBuzz i == "Fizz") $ \a ->
    forAll (arbitrary `suchThat` \i -> fizzBuzz i == "Buzz") $ \b ->
      fizzBuzz (a * b) == "FizzBuzz"

data Output = Fizz | Buzz deriving (Show, Eq, Ord)

instance Arbitrary Output where
  arbitrary = do
    i <- arbitrary :: Gen Int
    return $ if odd i then Fizz else Buzz

prop_fizzBuzz3 :: Output -> Output -> Property
prop_fizzBuzz3 o1 o2 =
  forAll (arbitrary `suchThat` \i -> fizzBuzz i == show o1) $ \a ->
    forAll (arbitrary `suchThat` \i -> fizzBuzz i == show o2) $ \b ->
      let expected = concatMap show $ nub $ sort [o1, o2] :: String
      in fizzBuzz (a * b) == expected

-- TESTING QuickCheck with functions

prop_partition :: [Integer] -> Fun Integer Bool -> Bool
prop_partition xs p' =
  let p = apply p'
      (lhs, rhs) = partition p xs
  in and
      [ all p lhs
      , not (any p rhs)
      , sort xs == sort (lhs ++ rhs) ]

prop_distributive :: Integer -> Integer -> Fun Integer Integer -> Bool
prop_distributive a b f = apply f (a + b) == apply f a + apply f b

-- Run tests :)

runTests :: IO ()
runTests = do
  quickCheck prop_fizzBuzz
  quickCheck prop_fizzBuzz2
  quickCheck prop_fizzBuzz3
  quickCheck prop_partition
  quickCheck prop_distributive

--
