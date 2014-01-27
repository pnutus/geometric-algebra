{-# LANGUAGE TemplateHaskell #-}

module Multivector 
  ( Multivector
  , geo
  , (•), dot
  , (§), out
  , mvZero, mvOne
  , mvReverse
  , mvSandwich
  , rotorBetween
  , e1, e2, e3, e1e2, e1e3, e2e3, e1e2e3
  ) where

import qualified Blades as B
import Blades (BasisBlade(..))
-- import qualified LinearCombination as LC
-- import LinearCombination (LinearCombination(..))
import Data.List (sort, find)
import Test.QuickCheck
import Test.QuickCheck.All
import Data.AEq
import Control.Monad

data Multivector = Mv [BasisBlade]
                 | Scalar Double
                 | Vec2 Double Double
                 | Bivec2 Double
                 | Rotor2 Double Double
                 | Vec3 Double Double Double
                 | Bivec3 Double Double Double
                 | Trivec3 Double
                 | Rotor3 Double Double Double Double
                 deriving (Eq)


-- * Operators

instance Num Multivector where
	(+) = add
	(*) = geo
	negate = mvNegate
	abs = mvAbs
	signum = mvNormalize
	fromInteger = mvFromScalar . fromInteger

instance Fractional Multivector where
	recip = mvReciprocal
	fromRational = mvFromScalar . fromRational

instance Floating Multivector where
  pi = mvFromScalar pi
  exp = mvExp
  sin = mvSin
  cos = mvCos
  cosh = mvCosh
  sinh = mvSinh
  log   = mvOnlyScalar log
  sqrt  = mvOnlyScalar sqrt
  acos  = mvOnlyScalar acos
  asin  = mvOnlyScalar asin
  atan  = mvOnlyScalar atan
  acosh = mvOnlyScalar acosh
  asinh = mvOnlyScalar asinh
  atanh = mvOnlyScalar atanh

tau = tupi
tupi :: Floating a => a
tupi = 2*pi  

mvOnlyScalar :: (Double -> Double) -> Multivector -> Multivector
mvOnlyScalar f x | isScalar x = mvFromScalar . f . getScalar $ x
                 | otherwise  = error "only works on scalars"
               

infixl 7 *>
(*>) :: Double -> Multivector -> Multivector
x *> (Mv blades) = Mv $ map (x B.*>) blades

infixl 6 +>
(+>) :: Double -> Multivector -> Multivector
x +> mv = (x *> mvOne) `add` mv

geo :: Multivector -> Multivector -> Multivector
geo x@(Mv _) y@(Mv _) = mvProduct B.geo x y
geo (Vec3 x1 y1 z1) (Vec3 x2 y2 z2)
  = Rotor3 (x1*x2 + y1*y2 + z1*z2) (x1*y2 - x2*y1) 
           (x1*z2 - x2*z1) (y1*z2 - y2*z1)

(•) = dot
dot :: Multivector -> Multivector -> Multivector
dot x@(Mv _) y@(Mv _) = mvProduct B.dot x y
dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Scalar (x1*x2 + y1*y2 + z1*z2)

(§) = out
out :: Multivector -> Multivector -> Multivector
out x@(Mv _) y@(Mv _) = mvProduct B.out x y
out (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Bivec3 (x1*y2 - x2*y1) 
          (x1*z2 - x2*z1) (y1*z2 - y2*z1)

mvProduct :: (BasisBlade -> BasisBlade -> BasisBlade) 
           -> Multivector -> Multivector -> Multivector
mvProduct op (Mv bs1) (Mv bs2) = bladeSum products
  where products = [b1 `op` b2 | b1 <- bs1, b2 <- bs2]

add :: Multivector -> Multivector -> Multivector
Mv bs1 `add` Mv bs2 = bladeSum $ foldr bladeAdd bs2 bs1

mvZero :: Multivector
mvZero = Mv []

mvNegate :: Multivector -> Multivector
mvNegate (Mv blades) = Mv $ map B.negate blades

mvSquare :: Multivector -> Multivector
mvSquare a@(Mv _) = a * mvReverse a

mvReciprocal :: Multivector -> Multivector
mvReciprocal x@(Mv _) = recip (getScalar $ mvSquare x) *> x 

mvOne :: Multivector
mvOne = Mv [B.one]

mvAbs :: Multivector -> Multivector
mvAbs = mvFromScalar . sqrt . getScalar . mvSquare

mvNormalize :: Multivector -> Multivector
mvNormalize a = a / abs a

mvReverse :: Multivector -> Multivector
mvReverse (Mv blades) = Mv $ map B.bladeReverse blades

mvInverse :: Multivector -> Multivector
mvInverse = mvReverse . mvReciprocal

mvSandwich :: Multivector -> Multivector -> Multivector
mvSandwich r a = r * a * mvInverse r

rotorBetween :: Multivector -> Multivector -> Multivector
rotorBetween a b = b * (a + b) / abs (a + b)

-- * Mathematical functions

-- | Multivector exponentiation. Implemented as a truncated Taylor series.
mvExp :: Multivector -> Multivector
mvExp x = mvTruncatedSeries [x^n / mvFactorial n | n <- [0..] ]

mvCos, mvSin :: Multivector -> Multivector
mvCos x = mvTruncatedSeries [(-1)^n * x^(2*n) / mvFactorial (2*n) | n <- [0..]]
mvSin x = mvTruncatedSeries 
  [(-1)^n * x^(2*n + 1) / mvFactorial (2*n + 1) | n <- [0..]]
  
mvCosh, mvSinh :: Multivector -> Multivector
mvCosh x = mvTruncatedSeries [x^(2*n)     / mvFactorial (2*n)     | n <- [0..]]
mvSinh x = mvTruncatedSeries [x^(2*n + 1) / mvFactorial (2*n + 1) | n <- [0..]]

mvTruncatedSeries :: [Multivector] -> Multivector
mvTruncatedSeries = sum . takeWhile large
  where large s = (getScalar $ abs s) > 1e-10

mvFactorial :: (Integral a) => a -> Multivector
mvFactorial n = fromIntegral $ product [1..n]

-- * Checks

isScalar :: Multivector -> Bool
isScalar (Mv [])    = True
isScalar (Mv [x])   = B.isScalar x
isScalar (Scalar _) = True
isScalar _          = False

isVersor :: Multivector -> Bool
isVersor (Mv x)           = and $ map (even . B.grade) x
isVersor (Scalar _)       = True
isVersor (Bivec2 _)       = True
isVersor (Rotor2 _ _)     = True
isVersor (Rotor3 _ _ _ _) = True
isVersor _                = False
        
-- * Conversions

mvGeneralize :: Multivector -> Multivector
mvGeneralize a@(Mv _) = a
mvGeneralize (Vec3 x y z) = x *> e1 + y *> e2 + z*>e3

mvFromScalar :: Double -> Multivector
mvFromScalar x = Mv [B.scalar x]

getScalar :: Multivector -> Double
getScalar (Mv blades) 
  | Just blade <- find B.isScalar blades = coeff blade
  | otherwise					    			         = 0

multivectorFromBasisBlade :: BasisBlade -> Multivector
multivectorFromBasisBlade blade = Mv [blade]

-- * Basis vectors

-- | Generates a basis vector, e.g. e_1.
e_ :: Int -> Multivector
e_ = multivectorFromBasisBlade . B.e_

e1 = e_ 1
e2 = e_ 2
e3 = e_ 3
e1e2 = e1*e2
e1e3 = e1*e3
e2e3 = e2*e3
e1e2e3 = e1e2*e3

-- * Printing

instance Show Multivector where
  show (Mv bs) = printTerms [(x,B.printBasis b) | BasisBlade b x <- bs]
  show (Scalar x) = printTerm True x ""
  show (Vec3 e1 e2 e3) = 
    printTerms [(e1,"e1"),(e2,"e2"),(e3,"e3")]
  show (Rotor3 x e12 e13 e23) = 
    printTerms [(x,""),(e12,"e1e2"),(e13,"e1e3"),(e23,"e2e3")]
  show (Bivec3 e12 e13 e23) = 
    printTerms [(e12,"e1e2"),(e13,"e1e3"),(e23,"e2e3")]

printTerms :: [(Double, String)] -> String
printTerms [] = "0"
printTerms ((x,b):ts) = concat $ first : rest
  where first = printTerm True x b 
        rest  = map (uncurry $ printTerm False) ts
        
printTerm :: Bool -> Double -> String -> String
printTerm first x basis = sign x ++ number ++ basis
  where sign x | first      = if x < 0 then "-" else ""
               | otherwise  = if x < 0 then " - " else " + "
        number = if absx == 1 && basis /= "" then "" else show absx
        absx   = abs x

-- * Blade stuff

bladeAdd :: BasisBlade -> [BasisBlade] -> [BasisBlade]
bladeAdd y [] = [y]
bladeAdd y@(BasisBlade b1 s1) (x@(BasisBlade b2 s2):xs)
  | b1 == b2      = if s == 0 then xs else BasisBlade b1 s : xs
  | b1 < b2       = y : x : xs
  | otherwise     = x : bladeAdd y xs
  where s = s1 + s2

bladeSimplify :: [BasisBlade] -> [BasisBlade]
bladeSimplify = foldr bladeAdd []

bladeSum :: [BasisBlade] -> Multivector
bladeSum = Mv . sort . bladeSimplify

-- * Comparisons

instance AEq Multivector where
  (===) = (==)
  (~==) = mvEqual

mvEqual :: Multivector -> Multivector -> Bool
(Mv bs1) `mvEqual` (Mv bs2) = and $ zipWith (~==) bs1 bs2 
        
-- * Testing

main = do runTests

runTests = $quickCheckAll

prop_associativity a b c = (a * b) * c ~== a * (b * c)
  where types = a :: Multivector

prop_leftDistributivity a b c = a * (b + c) ~== a * b + a * c
  where types = a :: Multivector

prop_rightDistributivity a b c = (a + b) * c ~== a * c + b * c
  where types = a :: Multivector

prop_revRev a = mvReverse (mvReverse a) == a

prop_invInv a = mvInverse (mvInverse a) ~== a

prop_revInv a = mvInverse (mvReverse a) ~== mvReverse (mvInverse a)

prop_normAbs a = (a /= mvZero) ==> abs (mvNormalize a) ~== mvOne

prop_rotate = forAll (vectorOf 2 $ liftM mvNormalize gen_vector) $ \[a,b] -> 
  let rotor = rotorBetween a b
  in mvSandwich rotor a ~== b

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