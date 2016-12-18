module Main where

import Criterion
import Data.Semigroup(Min(..))
import Lib
import RMQ

main :: IO ()
main = do
  someFunc
  testRMQ


testRMQ :: IO ()
testRMQ = do
  let rmq = fromList (map Min [3, 2, 1, 3, 1, 2, 4 :: Int])
  print $ 1 == query rmq (Range 0 7)
  print $ 2 == query rmq (Range 1 2)
  print $ 1 == query rmq (Range 1 3)
  print $ 1 == query rmq (Range 0 100)
  print $ query (emptyRmq :: RMQ (Min Int)) (Range 2 1)
