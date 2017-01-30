{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RapidCheck where

import Control.Monad
import Data.Monoid((<>))
import System.Random


--------------------------------------------------------------------------------
-- Simplified version of a Quick Check library
--------------------------------------------------------------------------------

newtype Gen a = Gen { runGen :: StdGen -> a }

newtype Property = Property { propGenerator :: Gen Result }

data Result
  = Success
  | Failure { seed :: Int, counterExample :: [String] }
  deriving (Show, Eq, Ord)

instance Monoid Result where
  mempty = Success
  mappend f@Failure{} _ = f
  mappend _ f@Failure{} = f
  mappend _ _ = Success

--------------------------------------------------------------------------------

class Arbitrary a where
  arbitrary :: Gen a

class CoArbitrary a where
  coarbitrary :: a -> Gen b -> Gen b

class Testable a where
  property :: a -> Property

instance Testable Property where
  property = id

instance Testable Result where
  property r = Property (Gen (const r))

instance Testable Bool where
  property = property . toResult where
      toResult b = if b then Success
                        else Failure { seed = 0, counterExample = []}


instance (Show a, Arbitrary a, Testable prop) => Testable (a -> prop) where
  property = forAll arbitrary


-- | For all values generated by `Gen a`, evaluate the property with the generated a

forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
forAll argGen prop =
  Property $ Gen $ \randGen ->
    let (randGen1, randGen2) = split randGen
        arg = runGen argGen randGen1
        propGen = propGenerator (property (prop arg))
    in case runGen propGen randGen2 of
      f@Failure{} -> f { counterExample = show arg : counterExample f }
      Success -> Success

rapidCheck :: Testable prop => prop -> IO Result
rapidCheck = rapidCheckWith 100

rapidCheckWith :: Testable prop => Int -> prop -> IO Result
rapidCheckWith attemptNb prop = do
  seed <- randomIO
  return $ rapidCheckImpl attemptNb seed prop

replay :: Testable prop => Result -> prop -> Result
replay Success _ = Success
replay f@Failure{} prop = rapidCheckImpl 1 (seed f) prop

rapidCheckImpl :: Testable prop => Int -> Int -> prop -> Result
rapidCheckImpl attemptNb startSeed prop = runAll (propGenerator (property prop))
  where
    runAll gen = foldMap (runOne gen) [startSeed .. startSeed + attemptNb - 1]
    runOne gen seed =
      case runGen gen (mkStdGen seed) of
        Success -> Success
        f@Failure{} -> f { seed = seed }

{-
instance Applicative Gen where
  pure a = Gen (const a)
  f <*> a =
    Gen $ \gen ->
      let (gen1, gen2) = split gen
      in (runGen f gen1) (runGen a gen2)

instance Monad Gen where
  a >>= f =
    Gen $ \gen ->
      let (gen1, gen2) = split gen
          b = runGen a gen1
      in runGen (f b) gen2

-- Alternative implementation based on the Applicative
runAttempts' :: Int -> Int -> Gen Result -> Result
runAttempts' attemptNb seed gen =
  let stdGen = mkStdGen seed
      gens = sequenceA (replicate attemptNb gen)
  in mconcat $ runGen gens stdGen
-}

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

instance Arbitrary Integer where
  arbitrary = Gen $ \gen -> fromIntegral $ fst (next gen)

instance CoArbitrary Integer where
  coarbitrary n (Gen g) = Gen $ \gen -> g (variant n gen)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Fun a b) where
  arbitrary = promote (\a -> coarbitrary a arbitrary)

variant :: (Integral n) => n -> StdGen -> StdGen
variant n randGen0 =
  foldl
    (\randGen b -> side b (split randGen))
    (side (n > 0) (split randGen0))
    (digits (abs n))
  where
    side b = if b then snd else fst
    digits =
      map ((== 0) . (`mod` 2))
      . takeWhile (> 0)
      . iterate (`div` 2)

promote :: (a -> Gen b) -> Gen (Fun a b)
promote f = Gen $ \gen -> Fun $ \a -> let g = f a in runGen g gen

data Fun a b = Fun { apply :: a -> b }
instance Show (Fun a b) where show _ = "<function>"

--------------------------------------------------------------------------------
-- Example
--------------------------------------------------------------------------------

prop_gcd :: Integer -> Integer -> Bool
prop_gcd a b = a * b == gcd a b * lcm a b

prop_gcd_bad :: Integer -> Integer -> Bool
prop_gcd_bad a b = gcd a b > 1

prop_composition :: Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool
prop_composition i (Fun f) (Fun g) = f (g i) == g (f i)

runTests :: IO ()
runTests = do
  print =<< rapidCheck prop_gcd
  failure <- rapidCheck prop_gcd_bad
  print failure
  print $ replay failure prop_gcd_bad
  print =<< rapidCheck prop_composition


--
