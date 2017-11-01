module Color where

import Data.Ratio
import Test.QuickCheck

data Color = Color { r, g, b, a :: Rational }
  deriving (Show, Eq, Ord)

-- data Color = Color { r, g, b, a :: Double } -- Fails
-- TODO: double multiplication is not associative,
--       BUT you can get it associative by composing * functions instead

over :: Color -> Color -> Color
over c1 c2 = Color
  { r = blend (r c1) (r c2)
  , g = blend (g c1) (g c2)
  , b = blend (b c1) (b c2)
  , a = a12 }
  where
    a1 = a c1
    a2 = a c2
    f1 = a1
    f2 = (1 - a1) * a2
    a12 = f1 + f2
    blend x y = (x * f1 + y * f2) / a12

posRatio :: Gen Rational
posRatio = arbitrary `suchThat` (\x -> x > 0)

instance Arbitrary Color where
  arbitrary = Color <$> posRatio <*> posRatio <*> posRatio <*> posRatio
  shrink (Color r g b a) =
    let shrink' = filter (> 0) . shrink
    in Color <$> shrink' r
             <*> shrink' g
             <*> shrink' b
             <*> shrink' a

prop_isOverCommutative :: Color -> Color -> Color -> Bool
prop_isOverCommutative c1 c2 c3 =
  over (over c1 c2) c3 == over c1 (over c2 c3)

--
