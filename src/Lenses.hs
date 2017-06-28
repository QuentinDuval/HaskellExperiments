{-# LANGUAGE RankNTypes #-}
module Lenses where

import Control.Lens
import Data.Char (toLower)
import Data.List
import qualified Data.Text as T
import Data.Text.Lens


test_data :: [(Int, String)]
test_data = [(1, "STRING"), (2, "INT"), (3, "LIST")]

test_data_text :: [(Int, T.Text)]
test_data_text = test_data & traversed . _2 %~ T.pack

test_data_rec :: [[(Int, String)]]
test_data_rec = [test_data, test_data]

isVowel :: Char -> Bool
isVowel x = elem (toLower x) "aeiou"


run_tests :: IO ()
run_tests = do

  -- Print all the elements of the list (with and without side effects)
  print (test_data ^.. traversed)
  test_data & traversed %%~ print
  print =<< (test_data & traversed %%~ print)

  -- Print all the second elements of the list (with and without side effects)
  print (test_data ^.. traversed . _2)
  test_data & traversed . _2 %%~ print

  -- Edit all first elements of the list (with and without side effects)
  print (test_data & traversed . _1 %~ (* 2))
  print =<< (test_data & traversed . _1 %%~ (return . (* 2)))

  -- Asks for the user input at every edit
  -- print =<< (test_data & traversed . _1 %%~ (\x -> do y <- readLn; return (x * y)))

  -- Change the string after the first letter toLower
  print (test_data & traversed . _2 . dropping 1 traversed %~ toLower)
  print (test_data_text & traversed . _2 . unpacked . dropping 1 traversed %~ toLower)
  print (test_data_text & traversed . _2 . dropping 1 text %~ toLower)

  -- Change the string after the first OVERALL letter toLower
  print (test_data & dropping 1 (traversed . _2 . traversed) %~ toLower)

  -- Reverse the strings (internally)
  print (test_data & traversed . _2 %~ reverse)
  print (test_data_rec & reversed . traversed . traversed . _2 %~ reverse)

  -- Reverse the strings (externally)
  -- let reversed = involuted reverse
  print (zip (test_data ^.. traversed . _1) (reverse (test_data ^.. traversed . _2)))
  print (zip (test_data ^.. traversed . _1) (test_data ^.. reversed . traversed . _2))

  -- Reverse the strings (externally - recursive)
  print $ test_data_rec & traversed %~ \x -> zip (x ^.. traversed . _1) (reverse (x ^.. traversed . _2))
  -- TODO: we are in trouble if we want to reverse the whole list accross all zooms (is this what Specter allows?)
  -- TODO: what about `backwards` in Control.Lens.Fold ??

  -- Reverse the vowel letters
  print ([1..10] & traversed . filtered even %~ (* 2))
  print (test_data ^.. traversed . _2 . traversed . filtered isVowel)
  -- TODO: how do you reverse the vowel internally ??? You need to be able to reconstruct the list (ISO ?)
  -- print (test_data & traversed . _2 %~ (\s -> ))

  -- Replace a sub-range with another range? It is doable, but we have to do it manually (check `srange`)
  print ([1..10] & srange 2 5 .~ [])
  print ([1..10] & srange 2 5 %~ reverse)
  print ([1..10] & srange 2 5 %~ concatMap (replicate 2))

  -- A list of trains with its coaches
  let trains = [(1, "abc"), (2, "edf")]
  print (concatMap (transposeOf _2) trains)
  print (concat (trains & traversed %~ transposeOf _2))
  -- print (trains ^.. transposeOf (traversed _2))

  -- Grouping duplicates using an isomorphism to [(nb, element)] <-> elements
  let str = "aaabbcdeeeefggh"
  print (str ^.. grouped)
  print (str & grouped . traversed . _1 %~ succ)
  print (str & grouped . traversed . _2 . from enum %~ succ)

  -- Filtering a range, and reversing this sub-range
  print ([1..10] & sfiltered odd %~ reverse)
  print ([1..10] & sfiltered even %~ reverse)

  return ()


{-
srange' :: Int -> Int ->  Iso' [a] [a]
srange' start end =
  iso
    (drop start . take (end - start))
    undefined -- How to reconstruct? We do not have enough data
-}

srangeImpl :: Int -> Int ->  Iso' [a] ([a], [a], [a])
srangeImpl start end =
  iso
    (\x -> (take start x, take (end - start) (drop start x) , drop end x))
    (\(before, x, after) -> before ++ x ++ after)

srange :: Int -> Int -> Lens' [a] [a]
srange start end = srangeImpl start end . _2

grouped :: (Eq a) => Iso' [a] [(Int, a)]
grouped = iso (foldr encode []) decode
  where
    encode x [] = [(1, x)]
    encode x (y:ys) = if x == snd y then (y & over _1 succ):ys else (1,x):y:ys
    decode = concatMap (\(n, x) -> replicate n x)

sfilteredImpl :: (a -> Bool) -> Iso' [a] ([Int], [a], [(Int, a)])
sfilteredImpl p = iso partitionIndex mergeIndex
  where
    partitionIndex xs =
      let (pos, neg) = partition (p . snd) (zip [0..] xs)
          (pis, pxs) = unzip pos
      in (pis, pxs, neg)
    mergeIndex (is, xs, ys) = map snd $ mergeBy (compare `on` fst) (zip is xs) ys

sfiltered :: (a -> Bool) -> Lens' [a] [a]
sfiltered p = sfilteredImpl p . _2



-- Helper functions

on :: (b -> b -> c) -> (a -> b) -> (a -> a -> c)
on f proj = \x y -> f (proj x) (proj y)

merge :: Ord a => [a] -> [a] -> [a]
merge = mergeBy compare

mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy cmp = loop
  where
    loop [] ys  = ys
    loop xs []  = xs
    loop (x:xs) (y:ys)
      = case cmp x y of
         GT -> y : loop (x:xs) ys
         _  -> x : loop xs (y:ys)

--
