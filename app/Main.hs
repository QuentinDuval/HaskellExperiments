module Main where

import Lib
import qualified RMQ

main :: IO ()
main = do
  someFunc
  RMQ.testRMQ
