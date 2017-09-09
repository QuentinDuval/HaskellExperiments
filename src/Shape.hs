{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Shape where

import Data.Monoid((<>))


newtype Shape coord = Shape { isInShape :: coord -> Bool }

type Point2D = (Double, Double)
type Point3D = (Double, Double, Double)
type Radius = Double
type Distance = Double

class EuclidianDistance coord where
  euclidianDistance :: coord -> coord -> Distance

instance EuclidianDistance Point2D where
  euclidianDistance (x1, y1) (x2, y2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

circle :: Point2D -> Radius -> Shape Point2D
circle center radius =
  Shape $ \coord -> euclidianDistance center coord <= radius

rectangle :: Point2D -> Point2D -> Shape Point2D
rectangle topRight@(right, top) bottomLeft@(left, bottom) =
  Shape $ \(x, y) -> and [x <= right, left <= x, y <= top, bottom <= y]

outside :: Shape Point2D -> Shape Point2D
outside s = Shape (not . isInShape s)

-- Different Monoids

full :: Shape coord
full = Shape (const True)

intersect :: Shape coord -> Shape coord -> Shape coord
intersect s1 s2 = Shape $ \coord -> all (`isInShape` coord) [s1, s2]

empty :: Shape coord
empty = Shape (const False)

superpose :: Shape coord -> Shape coord -> Shape coord
superpose s1 s2 = Shape $ \coord -> any (`isInShape` coord) [s1, s2]

-- Leveraging the Monoids

ring :: Point2D -> Radius -> Radius -> Shape Point2D
ring center smallRadius bigRadius =
  intersect
    (circle center bigRadius)
    (outside (circle center smallRadius))

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
