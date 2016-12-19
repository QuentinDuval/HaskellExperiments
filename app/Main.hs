module Main where

import Criterion
import Criterion.Main(defaultMain)
import Data.Semigroup(Min(..))
import Lib
import qualified RMQ
import RMQ(Range(..))
import qualified RMQ2


-- Run with:
-- stack exec Experiments-exe

main :: IO ()
main = do
  someFunc
  testRMQ
  defaultMain [testRMQPerf]


testRMQPerf :: Benchmark
testRMQPerf =
  let inputs = map Min [1..1000000 :: Int]
      rmq = RMQ.fromList inputs
      rmq2 = RMQ2.fromList inputs
  in bgroup "RMQ" [
      bench "Big queries" $ nf (RMQ.query rmq) (Range 100 10000),
      bench "Big queries" $ nf (RMQ2.query rmq2) (Range 100 10000)
    ]

-- For interactive use
testRMQ :: IO ()
testRMQ = do
  let rmq = RMQ.fromList (map Min [3, 2, 1, 3, 1, 2, 4 :: Int])
  print $ 1 == RMQ.query rmq (Range 0 7)
  print $ 2 == RMQ.query rmq (Range 1 2)
  print $ 1 == RMQ.query rmq (Range 1 3)
  print $ 1 == RMQ.query rmq (Range 0 100)
  print $ RMQ.query (RMQ.emptyRmq :: RMQ.RMQ (Min Int)) (Range 2 1)

  -- benchmark $ whnf (query rmq) (Range 2 5)
