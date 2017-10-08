{-# LANGUAGE BangPatterns #-}
module BinarySearch where

import qualified Data.Vector as V


-- Example for remainder

remainder :: Int -> Int -> Int
remainder = undefined

primes :: [Int]
primes = sieve [2..] where
  sieve (p:xs) = p : sieve (filter (\x -> rem x p /= 0) xs)

euclidianDiv :: Int -> Int -> (Int, Int) -- quotion and remainder
euclidianDiv = undefined

primeFactors :: Int -> Int
primeFactors = undefined -- TODO: double the search (quotient + remainer)

partitionPoint :: V.Vector a -> a -> (V.Vector a, V.Vector a) -- before and after
partitionPoint = undefined

--
