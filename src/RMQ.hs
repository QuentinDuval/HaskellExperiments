module RMQ (
  RMQ(size),
  Range(..),
  emptyRmq,
  singletonRmq,
  query,
  fromList
) where

import Data.Monoid((<>))
import Range

-- Range minimum query

data RMQ a
  = RMQ { size :: !Int, root :: PrivNode a }
  deriving (Show, Eq, Ord)

emptyRmq :: RMQ a
emptyRmq = RMQ { size = 0, root = Null }

singletonRmq :: a -> RMQ a
singletonRmq x = RMQ {
                  size = 1,
                  root = PrivNode { _val = x, _lhs = Null, _rhs = Null }
                }

query :: (Monoid a) => RMQ a -> Range -> a
query rmq = queryImpl (root rmq) (Range 0 (size rmq))

fromList :: (Monoid a) => [a] -> RMQ a
fromList = build


-- Private structure

data PrivNode a
  = Null
  | PrivNode {
    _val :: a,
    _lhs, _rhs :: PrivNode a }
  deriving (Show, Eq, Ord)


-- Query Impl

queryImpl :: (Monoid a) => PrivNode a -> Range -> Range -> a
queryImpl Null _ _ = mempty
queryImpl node (Range lo hi) range@(Range b e)
  | lo >= e || hi <= b = mempty
  | b <= lo && hi <= e = _val node
  | otherwise =
    let midPoint = div (lo + hi) 2
        lhsQuery = queryImpl (_lhs node) (Range midPoint hi) range
        rhsQuery = queryImpl (_rhs node) (Range lo midPoint) range
    in lhsQuery <> rhsQuery


-- Constructing from binary counter

build :: (Monoid a) => [a] -> RMQ a
build [] = emptyRmq
build xs = loop [] (map singletonRmq xs)
  where
    loop counter [] = foldr1 merge counter -- Order is crutial here
    loop [] (x:xs) = loop [x] xs
    loop counter@(c:cs) (x:xs)
      | size c >  size x = loop (x:counter) xs
      | size c == size x = loop cs (merge c x : xs)


merge :: (Monoid a) => RMQ a -> RMQ a -> RMQ a
merge a b = RMQ { size = size a + size b,
                  root = PrivNode {
                    _val = _val (root a) <> _val (root b),
                    _lhs = root a,
                    _rhs = root b }}

--
