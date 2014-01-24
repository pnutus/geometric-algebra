{-# LANGUAGE TemplateHaskell #-}

module Blades where

import Data.Word
import Data.Bits
import Data.List (intercalate)
import Data.Digits (digitsRev)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.All
import Data.AEq
import Prelude hiding (negate)

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
x `dot` y = gradeProj (x `geo` y) (abs $ bladeGrade x - bladeGrade y)  

out :: BasisBlade -> BasisBlade -> BasisBlade
x `out` y | (basis x).&.(basis y) /= 0   = scalar 0
          | otherwise                    = x `geo` y

geo :: BasisBlade -> BasisBlade -> BasisBlade
(BasisBlade b1 s1) `geo` (BasisBlade b2 s2) = BasisBlade bits coeff
     where bits = b1 `xor` b2
           coeff = s1 * s2 * canonicalSign b1 b2

reciprocal :: BasisBlade -> BasisBlade
reciprocal x = 1 / (coeff (x `geo` x)) *> x

bladeReverse :: BasisBlade -> BasisBlade
bladeReverse x | even permutation 	= x
			         | otherwise			    = negate x
			   where permutation = bladeGrade x `div` 2
			  
canonicalSign :: Bitmap -> Bitmap -> Double
canonicalSign 0 _   = 1
canonicalSign _ 0   = 1
canonicalSign b1 b2 | even minuses = 1
           			    | otherwise    = -1
  where minuses = sum $ map shiftand [1..(bitSize b2 - 1)] 
        shiftand n = popCount $ b1.&.(shift b2 n)

scalar :: Double -> BasisBlade
scalar x = BasisBlade 0 x

zero :: BasisBlade
zero = scalar 0

one :: BasisBlade
one = scalar 1

isScalar :: BasisBlade -> Bool
isScalar blade = basis blade == 0

isVector :: BasisBlade -> Bool
isVector blade = bladeGrade blade == 1

bladeGrade :: BasisBlade -> Int
bladeGrade blade = popCount (basis blade)

gradeProj :: BasisBlade -> Int -> BasisBlade
gradeProj blade grade 
  | bladeGrade blade == grade = blade
  | otherwise                 = zero

negate :: BasisBlade -> BasisBlade
negate (BasisBlade b s) = BasisBlade b (-s)

infixl 7 *>
(*>) :: Double -> BasisBlade -> BasisBlade
x *> (BasisBlade b s) = BasisBlade b (x*s)

e_ n = BasisBlade (bit $ n - 1) 1

basisVector n = bit (n - 1)

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
    where bitsToBool = map (/= 0)
        
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
  bladeGrade (outProd) == bladeGrade x + bladeGrade y
    where outProd = x `out` y

prop_geoGrade x y = 
  bladeGrade (x `geo` y) == bladeGrade x + bladeGrade y - 2*common
  where common = popCount $ basis x .&. basis y
  


instance Arbitrary BasisBlade where
  arbitrary = do
    scalar <- arbitrary
    bits <- arbitrary
    return (BasisBlade scalar bits)