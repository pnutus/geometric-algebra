{-# LANGUAGE TemplateHaskell #-}

module BasisBladeTests where

import Test.Framework.TH
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck hiding ( (.&.) )

import Data.AEq
import Data.Bits
import Numeric.GeometricAlgebra.BasisBlade
import Numeric.LinearCombination

tests = $(testGroupGenerator)

prop_outerAntisymmetry x y =
 isVector x && isVector y && (x /= y) ==> c == - c'
    where c  :* _ = x `out` y
          c' :* _ = y `out` x

prop_outerGrade x y = 
  (outProd /= 0) ==> grade outProd == grade x + grade y
    where _ :* outProd = x `out` y

prop_geoGrade x y = 
  grade geoProd == grade x + grade y - 2*common
    where common = popCount $ x .&. y
          _ :* geoProd = x `geo` y