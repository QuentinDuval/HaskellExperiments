{-# LANGUAGE BangPatterns #-}
module FiboRecur where

fiboRecur :: Int -> Integer
fiboRecur = loop 0 1
  where
    loop !curr !next n
      | n == 0    = curr
      | otherwise = loop next (curr + next) (n - 1)
