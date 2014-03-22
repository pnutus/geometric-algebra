module Numeric.GeometricAlgebra.Multivector where

import qualified Numeric.GeometricAlgebra.BasisBlade as B
import Numeric.GeometricAlgebra.BasisBlade (BasisBlade(..))
import qualified Numeric.LinearCombination as LC
import Numeric.LinearCombination (LinearCombination(..), Term(..))
import Numeric.GeometricAlgebra.Metric
import Data.List (find, transpose)
import GHC.Exts (sortWith)
import Data.AEq
import Data.List (intercalate)
import Data.Digits (digitsRev)
import Data.Bits ((.&.))

-- | Multivectors represent both geometric objects and operations in geometric
-- algebra. They are represented as a sum of basis blades, which isn't optimal
-- from a efficiency standpoint, but is very handy. Multivectors can be added,
-- multiplied (using the geometric, outer and verious inner products), 
-- exponentiated and more, as if they were just ordinary numbers.

data Multivector = Mv { metric :: Metric
                      , blades :: Blades
                      }
  deriving (Eq)
type Blades = LinearCombination Double BasisBlade


-- * Operators

instance Num Multivector where
	mv + mv' = metricCombine mv mv' $ blades mv + blades mv'
	(*) = geo
	negate = mapBlades negate
	abs = mvFromScalar . sqrt . getScalar . absq
	signum = normalize
	fromInteger = mvFromScalar . fromInteger

instance Fractional Multivector where
	recip x = mvReverse x * (mvFromScalar . recip . getScalar $ absq x)
	fromRational = mvFromScalar . fromRational

-- * Instance helpers

mapBlades :: (Blades -> Blades)
          -> Multivector -> Multivector
mapBlades f mv = Mv (metric mv) (f $ blades mv)

-- | 
metricCombine :: Multivector -> Multivector 
              -> Blades 
              -> Multivector
metricCombine mv mv' blades
  | isScalar mv     = Mv m' blades
  | isScalar mv'    = Mv m  blades
  | sameMetric      = Mv m  blades
  | otherwise       = error "Metrics have to be the same (except for scalars)."
  where sameMetric = m == m'
        m  = metric mv
        m' = metric mv'
        
        
-- | Geometric product. Same as '*'.
geo :: Multivector -> Multivector -> Multivector
geo = mvProduct B.geo

-- | Dot (inner) product.
dot :: Multivector -> Multivector -> Multivector
dot = mvProduct B.dot

-- | Dot (inner) product. Same as 'dot'. (alt-Q on mac)
infixl 9 •
(•) = dot

-- | Left contraction.
infixl 9 ⎦
(⎦) = mvProduct (B.⎦)

-- | Right contraction.
infixl 9 ⎣
(⎣) = mvProduct (B.⎣)

-- | Outer product. Same as '∧'.
out :: Multivector -> Multivector -> Multivector
out = mvProduct B.out

-- | Outer product. Same as 'out'.
infixl 8 ∧
(∧) = out

-- | Used to implement the different products.
mvProduct :: (BasisBlade -> BasisBlade -> Term Double BasisBlade) 
           -> Multivector -> Multivector -> Multivector
mvProduct op mv mv' = metricCombine mv mv' $ 
                      metricProduct op (metric mv) (blades mv) (blades mv')

-- * Metric products

metricProduct :: (BasisBlade -> BasisBlade -> Term Double BasisBlade) 
              -> Metric
              -> (Blades -> Blades -> Blades)
metricProduct op metric bs1 bs2 = case metric of
  EuclideanMetric          -> LC.multiplyUsing op bs1 bs2
  DiagonalMetric _ squares -> LC.multiplyUsing metricOp bs1 bs2 
    where metricOp = (metricBladeProduct squares op)
  Metric _ squares m       -> fromEigenmetric m $ bs'
    where bs' = LC.multiplyUsing metricOp bs1' bs2'
          metricOp = metricBladeProduct squares op
          [bs1', bs2'] = map (toEigenmetric m) [bs1, bs2]  

metricBladeProduct :: [Square]
              -> (BasisBlade -> BasisBlade -> Term Double BasisBlade)
              -> (BasisBlade -> BasisBlade -> Term Double BasisBlade)
metricBladeProduct squares op b b' = (factor * c) :* e
  where (c :* e) = b `op` b' 
        factor = product $ bitFilter (b .&. b') squares

fromEigenmetric :: MetricMatrix -> Blades -> Blades
fromEigenmetric m = toEigenmetric (transpose m)

toEigenmetric :: MetricMatrix -> Blades -> Blades
toEigenmetric m = LC.basisChangeUsing (metricBladeTransform m)
  
metricBladeTransform :: MetricMatrix -> BasisBlade -> Blades
metricBladeTransform m b = foldr step (LinComb [1 :* B.scalar]) rows
  where step row bs = LC.multiplyUsing B.out bs' bs
          where bs' = LinComb [c :* e | (c, e) <- zip row es, c /= 0 ]
        es = map B.e_ [1..]
        rows = bitFilter b m

-- | Multiplication by scalar.
infixl 7 *>
(*>) :: Double -> Multivector -> Multivector
x *> mv = mapBlades (LC.scalarMult x) mv

-- | Normalizes a 'Multivector' so that it squares to 1 or -1. Same as signum.
normalize :: Multivector -> Multivector
normalize a = a / abs a

-- | Reverses a 'Multivector'.
mvReverse :: Multivector -> Multivector
mvReverse = mapBlades (LC.operationUsing B.bladeReverse)

-- | Sandwiching a by r is the same as r * a * r^-1.
sandwich :: Multivector -> Multivector -> Multivector
sandwich r a = r * a * recip r

-- | Generates a rotor to rotate direction a into direction b.
rotorBetween :: Multivector -> Multivector -> Multivector
rotorBetween a b = b * normalize (a + b)


-- | Grade projection, the grade n part of a multivector A.
-- Same as 'gradeProject'.
infixl 9 <>
(<>) = gradeProject

-- | Synonym for '<>'.
gradeProject :: Multivector -> Int -> Multivector
gradeProject mv n = mapBlades (LC.filter (`B.isOfGrade` n)) mv

-- | The product of the squares of the constituent vector factors of a
-- Multivector.
absq :: Multivector -> Multivector
absq a = a * mvReverse a

-- * Mathematical functions

instance Floating Multivector where
  pi = mvFromScalar pi
  exp x  | isScalar x      = mapScalar exp x
         | hasPosSquare x  = cosh (abs x) + normalize x * sinh (abs x)
         | hasNegSquare x  = cos  (abs x) + normalize x * sin  (abs x)
         | hasNullSquare x = 1 + x
         | otherwise       = mvTruncatedSeries 
           [x^n / mvFactorial n | n <- [0..] ]
         
  cos x  | isScalar x      = mapScalar cos x
         | hasPosSquare x  = cos  (abs x)
         | hasNegSquare x  = cosh (abs x)
         | hasNullSquare x = 1 + x
         | otherwise       = mvTruncatedSeries 
           [(-1)^n * x^(2*n) / mvFactorial (2*n) | n <- [0..]]
                         
  sin x  | isScalar x      = mapScalar sin x
         | hasPosSquare x  = normalize x * sin  (abs x)
         | hasNegSquare x  = normalize x * sinh (abs x)
         | hasNullSquare x = 0
         | otherwise       = mvTruncatedSeries  
           [(-1)^n * x^(2*n+1) / mvFactorial (2*n+1) | n <- [0..]]
  
  cosh x | isScalar x      = mapScalar cosh x
         | hasPosSquare x  = cosh (abs x)
         | hasNegSquare x  = cos  (abs x)
         | hasNullSquare x = 1 + x
         | otherwise       = mvTruncatedSeries 
           [x^(2*n) / mvFactorial (2*n) | n <- [0..]]
  
  sinh x | isScalar x      = mapScalar sinh x
         | hasPosSquare x  = normalize x * sinh (abs x)
         | hasNegSquare x  = normalize x * sin  (abs x)
         | hasNullSquare x = 0
         | otherwise       = mvTruncatedSeries 
            [x^(2*n + 1) / mvFactorial (2*n + 1) | n <- [0..]]
  
  log x | isScalar x = mapScalar log x
        | isRotor  x = log (abs x) 
            + normalize (x <> 2) * mvAtan2 (abs $ x <> 2) (abs $ x <> 0) 
        | otherwise  = error "Only works with scalars and rotors."
  
  acos  = mvOnlyScalar acos
  asin  = mvOnlyScalar asin
  atan  = mvOnlyScalar atan
  acosh = mvOnlyScalar acosh
  asinh = mvOnlyScalar asinh
  atanh = mvOnlyScalar atanh

-- | The circle constant. Same as 2 * pi.
tupi, tau :: Floating a => a
tupi = 2 * pi  
tau = tupi

mvOnlyScalar :: (Double -> Double) -> Multivector -> Multivector
mvOnlyScalar f x | isScalar x = mapScalar f x
                 | otherwise  = error "Only works with scalars."

mapScalar :: (Double -> Double) -> Multivector -> Multivector
mapScalar f = mvFromScalar . f . getScalar

mvTruncatedSeries :: [Multivector] -> Multivector
mvTruncatedSeries = sum . takeWhile large
  where large s = (getScalar $ abs s) > 1e-10

mvFactorial :: (Integral a) => a -> Multivector
mvFactorial n = fromIntegral $ product [1..n]

mvAtan2 :: Multivector -> Multivector -> Multivector
mvAtan2 y x = mvFromScalar $ atan2 (getScalar y) (getScalar x)

-- * Predicates

isScalar :: Multivector -> Bool
isScalar 0  = True
isScalar mv = all B.isScalar (LC.elementsToList $ blades mv)

isVector :: Multivector -> Bool
isVector mv = all B.isVector (LC.elementsToList $ blades mv)

isBivector :: Multivector -> Bool
isBivector mv = all B.isBivector (LC.elementsToList $ blades mv)

isTrivector :: Multivector -> Bool
isTrivector mv = all B.isTrivector (LC.elementsToList $ blades mv)

isRotor :: Multivector -> Bool
isRotor mv = all B.isRotor (LC.elementsToList $ blades mv)

hasPosSquare :: Multivector -> Bool
hasPosSquare mv = hasScalarSquare mv && getScalar (mv^2) > 0

hasNegSquare :: Multivector -> Bool
hasNegSquare mv = hasScalarSquare mv && getScalar (mv^2) < 0

hasNullSquare :: Multivector -> Bool
hasNullSquare mv = hasScalarSquare mv && getScalar (mv^2) == 0

hasScalarSquare :: Multivector -> Bool
hasScalarSquare mv = isScalar $ mv^2

-- * Conversions

mvFromScalar :: Double -> Multivector
mvFromScalar 0 = Mv EuclideanMetric LC.zero
mvFromScalar x = Mv EuclideanMetric (LinComb [x :* B.scalar])

getScalar :: Multivector -> Double
getScalar mv 
  | Just (c :* _) <- LC.find B.isScalar (blades mv) = c
  | otherwise					    			                    = 0

-- * Basis vectors

-- | Generates basis vector n from a 'Metric'.
basisVector :: Metric -> Int -> Multivector
basisVector metric n = Mv metric $ LinComb [1 :* (B.e_ n)]

-- * Printing

instance Show Multivector where
  show mv = LC.showUsing showBlade (blades mv)
    where showBlade = showBladeWithNames (getNames . metric $ mv)

showBladeWithNames names blade = intercalate "∧" $ bitFilter blade names

-- | Converts a binary number to a list of ones and zeroes, beginning with the least significant bit.
--
-- >>> listOfBits 6
-- [0, 1, 1]
listOfBits :: (Integral a) => a -> [a]
listOfBits = digitsRev 2

-- | Filters a list using a bitmask. A combination of 'listOfBits' and 'maskFilter'.
--
-- >>> bitFilter 6 [1, 2, 3, 4, 5]
-- [2, 3]
bitFilter :: (Integral a) => a -> [b] -> [b]     
bitFilter bits = maskFilter (map boolFromBit . listOfBits $ bits)
    where boolFromBit = (== 1)

-- | Filters a list using another list of 'Boolean's (a mask). If one input list is short, excess elements of the longer list are discarded (same as for 'zip'). 
--
-- >>> maskFilter [False, True, True] [1, 2, 3, 4, 5]
-- [2, 3]
maskFilter :: [Bool] -> [a] -> [a] 
maskFilter mask xs = map snd $ filter fst (zip mask xs)


-- * Comparisons

instance AEq Multivector where
  (===) = (==)
  mv ~== mv' = metricEq && bladesEq
    where bladesEq = (blades mv) ~== (blades mv')
          metricEq = (metric mv) == (metric mv')
