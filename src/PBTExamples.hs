{-# LANGUAGE TypeFamilies #-}
module PBTExamples where

import Control.Arrow
import Data.List
import Data.Maybe
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Builder as Builder

import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Vector.Unboxed(Vector, (!))
import qualified Data.Vector.Unboxed as Vector

import Test.HUnit
import Test.QuickCheck


--------------------------------------------------------------------------------
-- First example
-- https://www.youtube.com/watch?v=XKu_SEDAykw
-- * The vector must be ordered
-- * Other cases (Hash map)
--------------------------------------------------------------------------------

pairSum :: Int -> Vector Int -> Maybe (Int, Int)
pairSum total ints = loop 0 (Vector.length ints - 1) where
  loop i j
    | i >= j = Nothing
    | otherwise =
      case compare (ints ! i + ints ! j) total of
        EQ -> Just (i, j)
        LT -> loop (succ i) j
        GT -> loop i (pred j)

hasPairSum :: Int -> Vector Int -> Bool
hasPairSum total = isJust . pairSum total

-- Unit tests

test_hasPairSum :: Test
test_hasPairSum = TestCase $ do
  assertEqual "Success case" True $ hasPairSum 8 (Vector.fromList [1, 3, 4, 4, 9])
  assertEqual "Success case" (Just (2, 3)) $ pairSum 8 (Vector.fromList [1, 3, 4, 4, 9])
  assertEqual "Failure case" False $ hasPairSum 8 (Vector.fromList [1, 3, 4, 6, 9])

-- Proof:
-- INVARIANT = distance to the right position if they exist
-- It can only go down

-- Property based testing

prop_findExistingSum :: Int -> Int -> [Int] -> Bool
prop_findExistingSum x y ints =
  hasPairSum (x + y) (Vector.fromList (sort (x : y : ints)))

prop_noExistingSum :: [Int] -> Property
prop_noExistingSum ints =
  let sums = Set.fromList [x + y | (x:ys) <- tails ints, y <- ys]
  in forAll arbitrary $ \total ->
    not (Set.member total sums) ==>
      not (hasPairSum total (Vector.fromList (sort ints)))


--------------------------------------------------------------------------------
-- Second example
-- Huffman Encoding
-- * The vector must be ordered
-- * Other cases (Hash map)
--------------------------------------------------------------------------------

data Heap k v = Heap {
  heapSize :: Int,
  heapData :: Map k [v] }
  deriving (Show, Eq, Ord)

emptyHeap :: (Ord k) => Heap k v
emptyHeap = Heap 0 Map.empty

makeHeap :: (Ord k, Foldable f) => f (k, v) -> Heap k v
makeHeap = foldl insertHeap emptyHeap

insertHeap :: (Ord k) => Heap k v -> (k, v) -> Heap k v
insertHeap (Heap s m) (k, v) =
  Heap (succ s) (Map.alter addKey k m)
  where
    addKey Nothing = pure [v]
    addKey (Just vs) = pure (v:vs)

popMin :: (Ord k) => Heap k v -> ((k, v), Heap k v)
popMin (Heap s m) =
  case Map.minViewWithKey m of
    Just ((k, [x]), m') -> ((k, x), Heap (pred s) m')
    Just ((k, (x:xs)), m') -> ((k, x), Heap (pred s) (Map.insert k xs m'))

popMins :: (Ord k) => Heap k v -> Int -> ([(k, v)], Heap k v)
popMins m 0 = ([], m)
popMins m n =
  let (kv, m') = popMin m
      (kvs, m'') = popMins m' (pred n)
  in (kv:kvs, m'')

-- Huffman encoding tree

type Freq = Int

data BinaryTree a
  = BinaryNode { lhs, rhs :: BinaryTree a }
  | BinaryLeaf { value :: a }

mergeHuffmanTree :: (Freq, BinaryTree a) -> (Freq, BinaryTree a) -> (Freq, BinaryTree a)
mergeHuffmanTree (w1, t1) (w2, t2) = (w1 + w2, BinaryNode t1 t2)

huffmanTree :: [(Freq, a)] -> BinaryTree a
huffmanTree = loop . makeHeap . fmap (second BinaryLeaf)
  where
    loop h
      | heapSize h < 2 = snd (fst (popMin h))
      | otherwise =
        let ([x, y], h') = popMins h 2
        in loop $ insertHeap h' (mergeHuffmanTree x y)

-- Huffman code

type Code = String

huffmanCode :: [(Freq, a)] -> [(a, Code)]
huffmanCode [] = []
huffmanCode freqs = treeToCode (huffmanTree freqs)
  where
    treeToCode (BinaryLeaf a) = [(a, "")]
    treeToCode (BinaryNode l r) =
      fmap (second ('0':)) (treeToCode l)
      ++ fmap (second ('1':)) (treeToCode r)

-- Encoding

encode :: (Foldable f, Monoid o) => (i -> Maybe o) -> f i -> o
encode encoding = foldMap (fromMaybe mempty . encoding)

-- Decoding

type Bit = Char

decode :: (Foldable f) => BinaryTree symbol -> f Bit -> [symbol]
decode huffTree = loop huffTree . foldr (:) []
  where
    loop _ [] = []
    loop (BinaryNode l r) (bit:bits) =
      case (if bit == '0' then l else r) of
        BinaryLeaf symbol -> symbol : loop huffTree bits
        nextTree -> loop nextTree bits

-- Proof : Show that the huffman encoding is minimal according to Shanon

-- Unit tests
-- * Encoding will likely test too much (check prefix...)
--   => YOU CAN USE PROPERTIES FOR TESTS AS WELL
-- * Back and Forth encoding...

test_huffmanCode :: Test
test_huffmanCode = TestCase $ do
  assertEqual "no symbol"
    ([] :: [(Char, Code)])
    (huffmanCode [])
  assertEqual "1 symbol"
    [('b', "")]
    (huffmanCode [(1, 'b')])
  assertEqual "3 symbols"
    [('b',"00"),('a',"01"),('c',"1")]
    (huffmanCode [(2, 'a'), (1, 'b'), (3, 'c')])

test_huffmanEncoding :: Test
test_huffmanEncoding = TestCase $ do
  let code = Map.fromList $ huffmanCode [(1, 'a'), (2, 'b'), (3, 'c')]
      encoder i = Map.lookup i code
  assertEqual "3 symbols" "00011" (encode encoder "abc")

test_huffmanDecoding :: Test
test_huffmanDecoding = TestCase $ do
  let decoding = huffmanTree [(1, 'a'), (2, 'b'), (3, 'c')]
  assertEqual "3 symbols" "abc" (decode decoding "00011")



-- Property based tests
-- * No prefix of other suffix
-- * Back and forth encoding

anyPrefixOfSuffix :: [String] -> Bool
anyPrefixOfSuffix xs =
  let sorted = sort xs
  in or (zipWith isPrefixOf sorted (drop 1 sorted))

prop_noPrefixOtherSuffix :: [(Freq, Char)] -> Bool
prop_noPrefixOtherSuffix =
  not . anyPrefixOfSuffix . fmap snd . huffmanCode

prop_encodeDecode :: [(Freq, Char)] -> Bool
prop_encodeDecode = undefined


--------------------------------------------------------------------------------
-- RUNNING ALL TESTS
--------------------------------------------------------------------------------

all_tests :: IO Counts
all_tests = runTestTT $
  TestList [ test_hasPairSum
           , test_huffmanCode
           , test_huffmanEncoding
           , test_huffmanDecoding ]

pbt_tests :: IO ()
pbt_tests = do
  quickCheck prop_findExistingSum
  quickCheck prop_noExistingSum
  quickCheck prop_noPrefixOtherSuffix

--
