{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Shape where

import Control.Monad
import Data.Monoid((<>))


newtype Shape coord = Shape { isInShape :: coord -> Bool }

type Coord2D = (Double, Double)
type Shape2D = Shape Coord2D
type Coord3D = (Double, Double, Double)
type Shape3D = Shape Coord3D

type Radius = Double
type Distance = Double

class EuclidianDistance coord where
  euclidianDistance :: coord -> coord -> Distance

instance EuclidianDistance Coord2D where
  euclidianDistance (x1, y1) (x2, y2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

circle :: Coord2D -> Radius -> Shape2D
circle center radius =
  Shape $ \coord -> euclidianDistance center coord <= radius

rectangle :: Coord2D -> Coord2D -> Shape2D
rectangle topRight@(right, top) bottomLeft@(left, bottom) =
  Shape $ \(x, y) -> and [x <= right, left <= x, y <= top, bottom <= y]

outside :: Shape coord -> Shape coord
outside s = Shape (not . isInShape s)

-- Different Monoids

allSpace :: Shape coord
allSpace = Shape (const True)

intersectAll :: Foldable f => f (Shape coord) -> Shape coord
intersectAll shapes = Shape $ \coord -> all (`isInShape` coord) shapes

intersect :: Shape coord -> Shape coord -> Shape coord
intersect s1 s2 = -- intersectAll [s1, s2]
  Shape $ \coord -> isInShape s1 coord && isInShape s2 coord

empty :: Shape coord
empty = Shape (const False)

superposeAll :: Foldable f => f (Shape coord) -> Shape coord
superposeAll shapes = Shape $ \coord -> any (`isInShape` coord) shapes

superpose :: Shape coord -> Shape coord -> Shape coord
superpose s1 s2 = superposeAll [s1, s2]

-- Leveraging the Monoids

ring :: Coord2D -> Radius -> Radius -> Shape2D
ring center smallRadius bigRadius =
  intersect
    (circle center bigRadius)
    (outside (circle center smallRadius))

--

test_shapes :: IO ()
test_shapes = do
  print "Circle (1, 1) 1:"
  forM_ [(2, 1), (2, 0)] $ \c -> do
    putStr (show c ++ " => ")
    print (isInShape (circle (1, 1) 1) c)
  print "Ring (1, 1) 1 2:"
  forM_ [(1, 1), (1, 2), (1, 3), (1, 4)] $ \c -> do
    putStr (show c ++ " => ")
    print (isInShape (ring (1, 1) 1 2) c)

--

{-
newtype Intersection coord = Intersection { unIntersect :: Shape coord }
newtype Superposition coord = Superposition { unSuperpose :: Shape coord }

intersection :: (coord -> Bool) -> Intersection coord
intersection = Intersection . Shape

superposition :: (coord -> Bool) -> Superposition coord
superposition = Superposition . Shape

instance Monoid (Intersection coord) where
  mempty = intersection (const True)
  mappend s1 s2 = mconcat [s1, s2]
  mconcat shapes = intersection $ \coord -> all (\(Intersection s) -> isInShape s coord) shapes

instance Monoid (Superposition coord) where
  mempty = superposition (const False)
  mappend s1 s2 = mconcat [s1, s2]
  mconcat shapes = superposition $ \coord -> any (\(Superposition s) -> isInShape s coord) shapes
-}

--
