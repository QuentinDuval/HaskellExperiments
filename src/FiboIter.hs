{-# LANGUAGE BangPatterns #-}
module FiboIter where

fiboIterate :: Int -> Integer
fiboIterate n = fst (iterate next (0, 1) !! n)
  where next !(!a, !b) = (b, a + b)
