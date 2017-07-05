{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
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
  print ([1..10] ^. sfiltered even)
  print ([1..10] & sfiltered even %~ reverse)
  print ([1..10] & sfiltered even . traversed %~ (* 2))

  -- sfiltered: what happens when you remove / add elements: no clear semantic
  -- TODO: Try with specter how it behaves (answer: badly)
  print ([1..10] & sfiltered even .~ [])             -- Looks kind of okay
  print ([1..10] & sfiltered even %~ tail)           -- Kind of MEH
  print ([1..10] & sfiltered even %~ (\x -> x ++ x)) -- Kind of MEH

  -- Chunked
  print ([1..10] ^.. chunked 4)
  print ([1..10] & chunked 4 . traversed %~ init) -- Drop every element
  print ([1..10] & chunked 4 . traversed %~ reverse) -- Reverse each chunk
  print ([1..10] & chunked 4 %~ reverse) -- Reverse chunks between them

  -- Combining them
  print ([1..20] ^.. srange 5 15 . sfiltered even . chunked 2)
  print ([1..20] & srange 5 15 . sfiltered even . chunked 2 %~ reverse)
  print ([1..20] & srange 5 15 . sfiltered even . traversed %~ (* 2))

  -- Combining transformations has to be done on RIGHT of %~
  print ([1..20] & srange 5 15 . sfiltered even %~ reverse . map (* 2))

  -- Combining zoom under a zoom
  let letters = (zip ['a'..'z'] [1..])
  print (letters & sfiltered (even . view _2) . from zipped . _1 %~ reverse)
  print (letters & partsOf (sfiltered (even . view _2) . traversed . _1) %~ reverse)

  -- FOLDS???

  return ()


-- RangeView

data RangeView a = RangeView { beforeRange, middleRange, afterRange :: [a] }

srangeImpl :: Int -> Int ->  Iso' [a] (RangeView a)
srangeImpl start end =
  iso
    (\x -> RangeView (take start x) (take (end - start) (drop start x)) (drop end x))
    (\x -> beforeRange x ++ middleRange x ++ afterRange x)

srange :: Int -> Int -> Lens' [a] [a]
srange start end =
  srangeImpl start end . lens middleRange (\v x -> v { middleRange = x })

grouped :: (Eq a) => Iso' [a] [(Int, a)]
grouped = iso (foldr encode []) decode
  where
    encode x [] = [(1, x)]
    encode x (y:ys) = if x == snd y then (y & over _1 succ):ys else (1,x):y:ys
    decode = concatMap (\(n, x) -> replicate n x)

data FilteredView a = FilteredView { selected :: [a] , unfiltered :: [a] }

sfilteredImpl :: (a -> Bool) -> Iso' [a] (FilteredView a)
sfilteredImpl p = iso toFilteredView fromFilteredView
  where
    toFilteredView xs = FilteredView (filter p xs) xs
    fromFilteredView v = loop (selected v) (unfiltered v)
      where
        loop [] unfiltered = filter (not . p) unfiltered
        loop selected [] = selected
        loop selected (u:unfiltered)
          | p u = head selected : loop (tail selected) unfiltered
          | otherwise = u : loop selected unfiltered

sfiltered :: (a -> Bool) -> Lens' [a] [a]
sfiltered p =
  sfilteredImpl p . lens selected (\v x -> v { selected = x })

chunked :: Int -> Iso' [a] [[a]]
chunked width = iso toChunks concat
  where
    toChunks = takeWhile (not . null)
               . map (take width)
               . iterate (drop width)

zipped :: Iso' ([a], [b]) [(a, b)]
zipped = iso (uncurry zip) unzip --TODO: in fact, partsOf can get rid of this

--
