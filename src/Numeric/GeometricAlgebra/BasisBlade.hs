module Numeric.GeometricAlgebra.BasisBlade where

import Data.Bits
import Data.AEq
import Data.Word
import Numeric.LinearCombination

type BasisBlade = Word

dot :: BasisBlade -> BasisBlade -> Term Double BasisBlade
x `dot` y = (x `geo` y) >>= (`gradeProject` (abs $ grade x - grade y))
  
(⎣) :: BasisBlade -> BasisBlade -> Term Double BasisBlade
x ⎣ y = (x `geo` y) >>= (`gradeProject` (grade x - grade y))

(⎦) :: BasisBlade -> BasisBlade -> Term Double BasisBlade
x ⎦ y = (x `geo` y) >>= (`gradeProject` (grade y - grade x))

out :: BasisBlade -> BasisBlade -> Term Double BasisBlade
x `out` y | (x .&. y) /= 0  = 0 :* scalar
          | otherwise       = x `geo` y

geo :: BasisBlade -> BasisBlade -> Term Double BasisBlade
b1 `geo` b2 = (normalOrderSign b1 b2) :* (b1 `xor` b2)
           
-- | Figures out the sign associated with the normal order of two 'BasisBlade's wedged together, e.g. @e1e3 * e2 -> -e1e2e3@.

normalOrderSign :: BasisBlade -> BasisBlade -> Double
normalOrderSign b b' | even (countSwaps b b') = 1
                     | otherwise              = -1
  where countSwaps 0 _  = 0
        countSwaps _ 0  = 0
        countSwaps b b' = popCount (b .&. shifted) + countSwaps b shifted
          where shifted = shift b' 1
  

bladeReverse :: BasisBlade -> Term Double BasisBlade
bladeReverse b | even permutation =   1  :* b
               | otherwise        = (-1) :* b
  where permutation = grade b `div` 2

isOfGrade :: BasisBlade -> Int -> Bool
blade `isOfGrade` n = grade blade == n

isScalar, isVector, isBivector, isTrivector:: BasisBlade -> Bool
isScalar    = (`isOfGrade` 0)
isVector    = (`isOfGrade` 1)
isBivector  = (`isOfGrade` 2)
isTrivector = (`isOfGrade` 3)
isRotor b   = isScalar b || isBivector b

grade :: BasisBlade -> Int
grade = popCount

gradeProject :: BasisBlade -> Int -> Term Double BasisBlade
gradeProject blade grade
  | blade `isOfGrade` grade = 1 :* blade
  | otherwise               = 0 :* scalar

(<>) :: BasisBlade -> Int -> Term Double BasisBlade
(<>) = gradeProject

scalar :: BasisBlade
scalar = 0

e_ n = bit $ n - 1

-- instance Show BasisBlade where
--   show blade = showIntAtBase 2 intToDigit blade ""