module Main where

import Criterion
import Criterion.Main(defaultMain)
import Data.Semigroup(Min(..))
import Lib
import RMQ


-- Run with:
-- stack exec Experiments-exe

main :: IO ()
main = do
  someFunc
  testRMQ
  defaultMain [testRMQPerf]


testRMQPerf :: Benchmark
testRMQPerf =
  let rmq = fromList (map Min [1..1000000 :: Int])
  in bgroup "RMQ" [
      bench "Big queries" $ nf (query rmq) (Range 100 10000) -- TODO: full eval?
    ]

-- For interactive use
testRMQ :: IO ()
testRMQ = do
  let rmq = fromList (map Min [3, 2, 1, 3, 1, 2, 4 :: Int])
  print $ 1 == query rmq (Range 0 7)
  print $ 2 == query rmq (Range 1 2)
  print $ 1 == query rmq (Range 1 3)
  print $ 1 == query rmq (Range 0 100)
  print $ query (emptyRmq :: RMQ (Min Int)) (Range 2 1)

  -- benchmark $ whnf (query rmq) (Range 2 5)
