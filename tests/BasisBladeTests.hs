{-# LANGUAGE TemplateHaskell #-}

module BasisBladeTests where

import Test.Framework.TH
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck hiding ( (.&.) )
import Numeric.BasisBlade
import Data.AEq
import Data.Bits

tests = $(testGroupGenerator)

prop_outerAntisymmetry x y =
 isVector x && isVector y && (basis x /= basis y) ==> 
  x `out` y ~== (-1) *> y `out` x

prop_geoOuterEquiv bl1@(BasisBlade b1 s1) bl2@(BasisBlade b2 s2) =
  (b1.&.b2 == 0) ==>
  bl1 `geo` bl2 == bl1 `out` bl2
  
prop_outerGrade x y = 
  (outProd /= scalar 0) ==>
  grade (outProd) == grade x + grade y
    where outProd = x `out` y

prop_geoGrade x y = 
  grade (x `geo` y) == grade x + grade y - 2*common
  where common = popCount $ basis x .&. basis y
  
instance Arbitrary BasisBlade where
  arbitrary = do
    scalar <- arbitrary
    bits <- arbitrary
    return (BasisBlade scalar bits)