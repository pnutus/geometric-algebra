{-# LANGUAGE TemplateHaskell #-}

module MultivectorTests where

import Test.Framework.TH
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Control.Monad (liftM)
import Data.List (sort)
import Data.AEq
import Numeric.GeometricAlgebra.Multivector
import Numeric.GeometricAlgebra.Metric
import Numeric.LinearCombination

tests = $(testGroupGenerator)

prop_associativity a b c = (a * b) * c ~== a * (b * c)
  where types = a :: Multivector

prop_leftDistributivity a b c = a * (b + c) ~== a * b + a * c
  where types = a :: Multivector

prop_rightDistributivity a b c = (a + b) * c ~== a * c + b * c
  where types = a :: Multivector

prop_revRev a = mvReverse (mvReverse a) == a

prop_invInv a = recip (recip a) ~== a
  where types = a :: Multivector

prop_revInv a = recip (mvReverse a) ~== mvReverse (recip a)

prop_normAbs a = (a /= 0) ==> abs (normalize a) ~== 1

prop_rotate = forAll (vectorOf 2 $ liftM normalize gen_vector) $ \[a,b] -> 
  let rotor = rotorBetween a b
  in sandwich rotor a ~== b

-- * generators

gen_vector = do 
  list <- sqrtArbitrary
  return $ vectorFromComponents list

instance Arbitrary Multivector where
  arbitrary = do
    blades <- arbitrary
    return $ Mv EuclideanMetric blades

instance (AEq a, Num a, Arbitrary a, Ord b, Arbitrary b) 
      => Arbitrary (LinearCombination a b) where
  arbitrary = do
    terms <- sqrtArbitrary
    return . simplify . LinComb $ terms

instance (Arbitrary a, Arbitrary b) => Arbitrary (Term a b) where
  arbitrary = do
    c <- arbitrary
    e <- arbitrary
    return $ c :* e

sqrtArbitrary :: (Arbitrary a) => Gen a
sqrtArbitrary = sized $ \n -> resize (intSqrt n) arbitrary
  where intSqrt = round . sqrt . fromIntegral