{-# LANGUAGE TemplateHaskell #-}

module Blades where

import Data.Word
import Data.Bits
import Data.List (intercalate)
import Data.Digits (digitsRev)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.All
import Data.AEq
import Prelude hiding (negate, reverse)

type Bitmap = Word8
data BasisBlade = BasisBlade { basis :: Bitmap
                             , coeff :: Double 
                             } 
                             deriving (Eq, Ord)



instance AEq BasisBlade where
  (BasisBlade b1 s1) === (BasisBlade b2 s2) = 
    b1 == b2 && s1 === s2
  (BasisBlade b1 s1) ~== (BasisBlade b2 s2) = 
    b1 == b2 && s1 ~== s2
    
dot :: BasisBlade -> BasisBlade -> BasisBlade
x `dot` y = gradeProject (x `geo` y) (abs $ grade x - grade y)  

(⎣) :: BasisBlade -> BasisBlade -> BasisBlade
x ⎣ y = gradeProject (x `geo` y) (grade x - grade y)

(⎦) :: BasisBlade -> BasisBlade -> BasisBlade
x ⎦ y = gradeProject (x `geo` y) (grade y - grade x)

out :: BasisBlade -> BasisBlade -> BasisBlade
x `out` y | (basis x).&.(basis y) /= 0   = scalar 0
          | otherwise                    = x `geo` y

geo :: BasisBlade -> BasisBlade -> BasisBlade
(BasisBlade b1 s1) `geo` (BasisBlade b2 s2) = BasisBlade bits coeff
     where bits = b1 `xor` b2
           coeff = s1 * s2 * canonicalSign b1 b2
           


reverse :: BasisBlade -> BasisBlade
reverse x | squareSign x == 1 = x
          | otherwise         = negate x

negate :: BasisBlade -> BasisBlade
negate (BasisBlade b s) = BasisBlade b (-s)

canonicalSign :: Bitmap -> Bitmap -> Double
canonicalSign 0 _   = 1
canonicalSign _ 0   = 1
canonicalSign b1 b2 | even minuses = 1
           			    | otherwise    = -1
  where minuses = sum $ map shiftand [1..(bitSize b2 - 1)] 
        shiftand n = popCount $ b1.&.(shift b2 n)


isOfGrade :: Int -> BasisBlade -> Bool
isOfGrade n blade = grade blade == n

isScalar, isVector, isBivector, isTrivector:: BasisBlade -> Bool
isScalar    = isOfGrade 0
isVector    = isOfGrade 1
isBivector  = isOfGrade 2
isTrivector = isOfGrade 3

squareSign :: BasisBlade -> Int
squareSign x | even permutation = 1
             | odd  permutation = -1
  where permutation = grade x `div` 2

grade :: BasisBlade -> Int
grade blade = popCount (basis blade)

gradeProject :: BasisBlade -> Int -> BasisBlade
gradeProject blade g 
  | grade blade == g  = blade
  | otherwise         = scalar 0

infixl 7 *>
(*>) :: Double -> BasisBlade -> BasisBlade
x *> (BasisBlade b s) = BasisBlade b (x*s)

scalar :: Double -> BasisBlade
scalar x = BasisBlade 0 x

e_ n = BasisBlade (bit $ n - 1) 1

-- Printing

instance Show BasisBlade where
  show (BasisBlade 0 s) = show s
  show (BasisBlade b s) = coeff ++ printBasis b
    where coeff = case s of 
                       1    -> ""
                       (-1) -> "-"
                       _    -> show s

printBasis basis = intercalate "e" $ "" : map show (bitFilter basis [1..])
     
bitFilter bits = maskFilter (bitsToBool $ digitsRev 2 bits)
    where bitsToBool = map (== 1)
        
maskFilter mask xs = map snd $ filter fst (zip mask xs)

-- Tests/Properties

runTests = $quickCheckAll

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