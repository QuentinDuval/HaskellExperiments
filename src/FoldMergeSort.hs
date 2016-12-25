{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module FoldMergeSort (
  foldMergeSort,
  foldMergeSort',
  testFoldMergeSort,
) where

import Data.Monoid


-- Implementing it as a real FOLD

foldMergeSort :: (Ord a) => [a] -> [a]
foldMergeSort =
  foldl1 (flip merge) . map snd . foldl addToCounter []
  where
    addToCounter counter x = propagate ((1::Int,[x]) : counter)
    propagate [] = []
    propagate [x] = [x]
    propagate counter@(x:y:xs) -- x arrived last => combine on right
      | fst x == fst y = propagate ((fst x + fst y, merge (snd y) (snd x)) : xs)
      | otherwise      = counter


-- Folding any Monoid

foldMonoidTree :: (Monoid a) => [a] -> a
foldMonoidTree =
  foldl1 (flip (<>)) . map snd . foldl addToCounter []
  where
    addToCounter counter x = propagate ((1::Int,x) : counter)
    propagate [] = []
    propagate [x] = [x]
    propagate counter@(x:y:xs) -- x arrived last => combine on right
      | fst x == fst y = propagate ((fst x + fst y, snd y <> snd x) : xs)
      | otherwise      = counter

foldMergeSort' :: (Ord a) => [a] -> [a]
foldMergeSort' = unSortedList . foldMonoidTree . map (\x -> SortedList [x])


-- SortedList Monoid

newtype SortedList a = SortedList { unSortedList :: [a] }

instance (Ord a) =>  Monoid (SortedList a) where
  mempty = SortedList []
  mappend (SortedList a) (SortedList b) = SortedList $ merge a b


-- Utils

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge l@(x:xs) r@(y:ys)
  | y < x     = y : merge l ys -- Keep it stable (only if strictly less)
  | otherwise = x : merge xs r


-- Test stability

data Obj = Obj Int Int deriving (Show, Eq)

instance Ord Obj where
  compare (Obj a _) (Obj b _) = compare a b



testFoldMergeSort :: IO ()
testFoldMergeSort = do
  let inputs = [2, 4, 1, 3, 1, 5, 2 :: Int]
  print $ foldMergeSort inputs
  print $ foldMergeSort' inputs

  let stables = [Obj 2 3, Obj 1 1, Obj 1 2, Obj 2 1, Obj 2 2]
  print $ foldMergeSort stables
  print $ foldMergeSort' stables
