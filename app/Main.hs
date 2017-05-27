module Main where

import Criterion
import Criterion.Main(defaultMain)
import Data.Semigroup(Min(..))
import qualified Fibonacci
import qualified RMQ
import RMQ(Range(..))
import qualified RMQ2
import qualified Tree


-- Run with:
-- stack exec Experiments-exe

main :: IO ()
main = do
  testRMQ
  defaultMain [
      -- testTreePerf,
      -- testRMQPerf,
      testFiboPerf
    ]

testFiboPerf :: Benchmark
testFiboPerf =
  bgroup "Fibo" [
    bench "Iterate" $ nf Fibonacci.fiboIterate 1000,
    bench "Recur" $ nf Fibonacci.fiboRecur 1000
    -- bench "Reduce" $ nf sum [1..1000 :: Int]
    ]

testTreePerf :: Benchmark
testTreePerf =
  let n = 1000 :: Int
      t = Tree.Tree $ Tree.Node 0 $ map Tree.generateNode [1,6..n]
      d = Tree.Tree $ Tree.generateDepth n
  in bgroup "Tree" [
      bgroup "Flat" [
        bench "Naive" $ nf Tree.treeWalkR t,
        bench "Heap" $ nf Tree.treeWalkH t,
        bench "CPS" $ nf Tree.treeWalkC t,
        bench "Cont" $ nf Tree.treeWalkM t
      ],
      bgroup "Deep" [
        bench "Naive" $ nf Tree.treeWalkR d,
        bench "Heap" $ nf Tree.treeWalkH d,
        bench "CPS" $ nf Tree.treeWalkC d,
        bench "Cont" $ nf Tree.treeWalkM d
      ]
     ]


testRMQPerf :: Benchmark
testRMQPerf =
  let n = 1000000 :: Int
      inputs = map Min [1..n]
      rmq = RMQ.fromList inputs
      rmq2 = RMQ2.fromList inputs
  in bgroup "RMQ" [
      bench "Query - Tree based" $ nf (RMQ.query rmq) (Range 100 10000),
      bench "Build - Tree based" $ nf (RMQ.query (RMQ.fromList inputs)) (Range 0 n),
      bench "Query - Vect based" $ nf (RMQ2.query rmq2) (Range 100 10000),
      bench "Build - Vect based" $ nf (RMQ2.query (RMQ2.fromList inputs)) (Range 0 n)
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
