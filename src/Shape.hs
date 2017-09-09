{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Shape where


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

instance Monoid (Shape coord) where
  mempty = Shape (const False)
  mappend s1 s2 = mconcat [s1, s2]
  mconcat shapes = Shape $ \coord -> any (`isInShape` coord) shapes

--
