{-# LANGUAGE TypeFamilies, DeriveFunctor #-}
module PBTExamples where

import Control.Arrow
import Control.Monad.Identity
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Foldable(for_)
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
  True  @=? hasPairSum 8 (Vector.fromList [1, 3, 4, 4, 9])
  False @=? hasPairSum 8 (Vector.fromList [1, 3, 4, 6, 9])
  Just (2, 3) @=? pairSum 8 (Vector.fromList [1, 3, 4, 4, 9])


-- Proof:
-- INVARIANT = distance to the right position if they exist
-- It can only go down

-- Property based testing

prop_findExistingSum :: Int -> Int -> [Int] -> Bool
prop_findExistingSum x y ints =
  let sortedInts = Vector.fromList (sort (x : y : ints))
  in case pairSum (x + y) sortedInts of
      Nothing -> False
      Just (i, j) -> x + y == sortedInts ! i + sortedInts ! j

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
type Code = String

data BinaryTree a
  = BinaryNode { lhs, rhs :: BinaryTree a }
  | BinaryLeaf { value :: a }
  deriving (Show, Eq, Ord, Functor)

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

treeToCode :: BinaryTree symbol -> [(symbol, Code)]
treeToCode (BinaryLeaf a) = [(a, "")]
treeToCode (BinaryNode l r) =
  fmap (second ('0':)) (treeToCode l) ++ fmap (second ('1':)) (treeToCode r)

-- Encoding / Decoding

type Bit = Char

toEncoder :: (Monad m, Ord symbol) => BinaryTree symbol -> Conduit symbol m Bit
toEncoder huffTree =
  let encoding = Map.fromList (treeToCode huffTree)
  in CL.mapFoldable $ \i -> fromMaybe [] (Map.lookup i encoding)

toDecoder :: (Monad m) => BinaryTree symbol -> Conduit Bit m symbol
toDecoder huffTree = loop huffTree
  where
    loop currTree@(BinaryNode l r) = do
      bit <- await
      case bit of
        Nothing -> pure ()
        Just bit -> case (if bit == '0' then l else r) of
          BinaryLeaf symbol -> yield symbol >> loop huffTree
          nextTree -> loop nextTree

encode :: (Foldable f) => Conduit i Identity o -> f i -> [o]
encode decoder inputs = runConduitPure $
  for_ inputs yield .| decoder .| CL.consume


-- Proof : Show that the huffman encoding is minimal according to Shanon

-- Unit tests
-- * Encoding will likely test too much (check prefix...)
--   => YOU CAN USE PROPERTIES FOR TESTS AS WELL
-- * Back and Forth encoding...

test_huffmanCode :: Test
test_huffmanCode = TestCase $ do
  [('b', "")] @=? treeToCode (huffmanTree [(1, 'b')])
  [('b',"00"),('a',"01"),('c',"1")] @=? treeToCode (huffmanTree [(2, 'a'), (1, 'b'), (3, 'c')])

test_huffmanEncoding :: Test
test_huffmanEncoding =
  let huffTree = huffmanTree [(1, 'a'), (2, 'b'), (3, 'c')]
      encoder = toEncoder huffTree
      decoder = toDecoder huffTree
  in TestCase $ do
      "00011" @=? encode encoder "abc"
      "abc" @=? encode decoder "00011"


-- Property based tests
-- * No prefix of other suffix
-- * Back and forth encoding

anyPrefixOfSuffix :: [String] -> Bool
anyPrefixOfSuffix xs =
  let sorted = sort xs
  in or (zipWith isPrefixOf sorted (drop 1 sorted))

prop_noPrefixOtherSuffix :: [(Freq, Char)] -> Property
prop_noPrefixOtherSuffix freqs =
  length freqs > 1 ==>
    not $ anyPrefixOfSuffix $ fmap snd (treeToCode $ huffmanTree freqs)

prop_encodeDecode :: [(Freq, Char)] -> Property
prop_encodeDecode freqs =
  length freqs > 1 ==>
    let chars = map snd freqs
        decoder = toDecoder (huffmanTree freqs)
        encoder = toEncoder (huffmanTree freqs)
    in forAll (listOf (elements chars)) $ \text ->
        encode decoder (encode encoder text) == text


--------------------------------------------------------------------------------
-- RUNNING ALL TESTS
--------------------------------------------------------------------------------

all_tests :: IO Counts
all_tests = runTestTT $
  TestList [ test_hasPairSum
           , test_huffmanCode
           , test_huffmanEncoding ]

pbt_tests :: IO ()
pbt_tests = do
  quickCheck prop_findExistingSum
  quickCheck prop_noExistingSum
  quickCheck prop_noPrefixOtherSuffix
  quickCheck prop_encodeDecode

--
