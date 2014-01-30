{-# LANGUAGE TemplateHaskell #-}

module MultivectorTests where

import Numeric.Multivector
import Test.Framework.TH
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Control.Monad (liftM)
import Data.List (sort)
import Data.AEq
import BasisBladeTests

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

prop_sorted a@(Mv blades) = a == Mv (sort blades) 

-- * generators
  
gen_vector = do 
  (x,y,z) <- arbitrary
  return (x *> e1 + y *> e2 + z *> e3)

instance Arbitrary Multivector where
  arbitrary = do
    blades <- sized $ \n -> resize (intSqrt n) arbitrary
    return . Mv . bladeSimplify $ blades
    where intSqrt = round . sqrt . fromIntegral