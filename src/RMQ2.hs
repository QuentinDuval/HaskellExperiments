module RMQ2 (
  RMQ(width),
  fromList,
  query,
) where

import Data.Monoid((<>))
import qualified Data.Vector as V
import Range


-- Range minimum query type

data RMQ a = RMQ { width :: !Int, _tree :: V.Vector a }

-- Construction

fromList :: (Monoid a) => [a] -> RMQ a
fromList xs =
    let l = length xs
        width = until (>= l) (*2) 1
        buildFloor = map mconcat . chunksOf 2
        addFloor xs = buildFloor (head xs) : xs
        isRoot xs = case xs of ([x]:_) -> True; _ -> False
        floors = until isRoot addFloor [xs ++ replicate (width - l) mempty]
    in RMQ { width = width
           , _tree = V.fromList (concat floors) }


-- Query

query :: (Monoid a) => RMQ a -> Range -> a
query (RMQ width tree) (Range lo hi) = queryImpl 0 0 width -- [lo, hi) is open searched range
    where
      queryImpl node l r -- Current node index + [start, end) of current range
        | r <= lo || hi <= l = mempty
        | lo <= l && r <= hi = tree V.! node
        | otherwise = queryImpl lhs l mid <> queryImpl rhs mid r
        where
          lhs = 2 * node + 1 -- left node
          rhs = 2 * node + 2 -- right node
          mid = div (l + r) 2


-- Private

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs = let (a, b) = splitAt n xs in a : chunksOf n b
