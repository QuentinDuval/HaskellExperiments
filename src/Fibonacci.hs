module Fibonacci where

fiboIterate :: Int -> Integer
fiboIterate n = fst (iterate next (0, 1) !! n)
  where next (a, b) = (b, a + b)

fiboRecur :: Int -> Integer
fiboRecur = loop 0 1
  where
    loop curr next n
      | n == 0    = curr
      | otherwise = loop next (curr + next) (n - 1)
