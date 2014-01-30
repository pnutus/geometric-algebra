-- | Geometric algebra module optimized for understanding, not efficiency.
-- In GA, both objects and operators are 'Multivector's that can be added
-- multiplied, exponentiated with expected results.
-- For more information on GA, see the
-- <http://en.wikipedia.org/wiki/Geometric_algebra Wikipedia article> on GA
-- (including referenced literature).

module Numeric.Multivector where

import qualified Numeric.BasisBlade as B
import Numeric.BasisBlade (BasisBlade(..))
import Data.List (find)
import GHC.Exts (sortWith)
import Data.AEq

-- | Multivectors represent both geometric objects and operations in geometric
-- algebra. They are represented as a sum of basis blades, which isn't optimal
-- from a efficiency standpoint, but is very handy. Multivectors can be added,
-- multiplied (using the geometric, outer and verious inner products), 
-- exponentiated and more, as if they were just ordinary numbers.

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
	(Mv bs1) + (Mv bs2) = let [short, long] = sortWith length [bs1, bs2] 
                        in  Mv $ foldr bladeAdd long short
	(*) = geo
	negate (Mv blades) = Mv $ map B.negate blades
	abs = mvFromScalar . sqrt . getScalar . mvSquare
	signum = normalize
	fromInteger = mvFromScalar . fromInteger

instance Fractional Multivector where
	recip x = mvReverse x * (mvFromScalar . recip . getScalar $ mvSquare x)
	fromRational = mvFromScalar . fromRational

-- | Geometric product. Same as '*'.
geo :: Multivector -> Multivector -> Multivector
geo x@(Mv _) y@(Mv _) = mvProduct B.geo x y
geo (Vec3 x1 y1 z1) (Vec3 x2 y2 z2)
  = Rotor3 (x1*x2 + y1*y2 + z1*z2) (x1*y2 - x2*y1) 
           (x1*z2 - x2*z1) (y1*z2 - y2*z1)


-- | Dot (inner) product.
dot :: Multivector -> Multivector -> Multivector
dot x@(Mv _) y@(Mv _) = mvProduct B.dot x y
dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Scalar (x1*x2 + y1*y2 + z1*z2)

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
out x@(Mv _) y@(Mv _) = mvProduct B.out x y
out (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Bivec3 (x1*y2 - x2*y1) 
          (x1*z2 - x2*z1) (y1*z2 - y2*z1)

-- | Outer product. Same as 'out'.
infixl 8 ∧
(∧) = out

-- | Used to implement the different products.
mvProduct :: (BasisBlade -> BasisBlade -> BasisBlade) 
           -> Multivector -> Multivector -> Multivector
mvProduct op (Mv bs1) (Mv bs2) = Mv $ bladeSimplify products
  where products = [result | b1 <- bs1, b2 <- bs2, 
                    let result = b1 `op` b2,
                    B.coeff result /= 0]

-- | Multiplication by scalar.
infixl 7 *>
(*>) :: Double -> Multivector -> Multivector
x *> (Mv blades) = Mv $ map (x B.*>) blades

-- | Addition by scalar.
infixl 6 +>
(+>) :: Double -> Multivector -> Multivector
x +> mv = mvFromScalar x + mv

-- | Normalizes a 'Multivector' so that it squares to 1 or -1. Same as signum.
normalize :: Multivector -> Multivector
normalize a = a / abs a

-- | Reverses a 'Multivector'.
mvReverse :: Multivector -> Multivector
mvReverse (Mv blades) = Mv $ map B.reverse blades

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
gradeProject (Mv blades) n = Mv $ filter (B.isOfGrade n) blades

-- | The product of the squares of the constituent vector factors of a
-- Multivector.
mvSquare :: Multivector -> Multivector
mvSquare a@(Mv _) = a * mvReverse a

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
isScalar (Mv [])    = True
isScalar (Mv [x])   = B.isScalar x
isScalar (Scalar _) = True
isScalar _          = False

isVector :: Multivector -> Bool
isVector (Mv blades)  = all B.isVector blades
isVector (Vec2 _ _)   = True
isVector (Vec3 _ _ _) = True
isVector _            = False

isBivector :: Multivector -> Bool
isBivector (Mv blades)    = all B.isBivector blades
isBivector (Bivec2 _)     = True
isBivector (Bivec3 _ _ _) = True
isBivector _              = False

isTrivector :: Multivector -> Bool
isTrivector (Mv blades)   = all B.isTrivector blades
isTrivector (Trivec3 _)   = True
isTrivector _             = False

isRotor :: Multivector -> Bool
isRotor (Mv blades)       = all (\b -> B.isScalar b || B.isBivector b) blades
isRotor (Rotor2 _ _)      = True
isRotor (Rotor3 _ _ _ _)  = True
isRotor _                 = False

hasPosSquare :: Multivector -> Bool
hasPosSquare (Mv blades)      = all ((1 ==) . B.squareSign) blades
hasPosSquare (Scalar _)       = True
hasPosSquare (Vec2 _ _)       = True
hasPosSquare (Vec3 _ _ _)     = True
hasPosSquare _                = False

hasNegSquare :: Multivector -> Bool
hasNegSquare (Mv blades)      = all ((-1 ==) . B.squareSign) blades
hasNegSquare x                = isBivector x || isTrivector x

hasNullSquare :: Multivector -> Bool
hasNullSquare (Mv [])     = True
hasNullSquare (Scalar 0)  = True
hasNullSquare _           = False

hasScalarSquare :: Multivector -> Bool
hasScalarSquare x = hasPosSquare x || hasNegSquare x || hasNullSquare x

-- * Conversions

-- | Creates arbitrary Multivectors from specialized types.
mvGeneralize :: Multivector -> Multivector
mvGeneralize a@(Mv _) = a
mvGeneralize (Scalar x) = x *> 1
mvGeneralize (Vec2 x y) = x *> e1 + y *> e2
mvGeneralize (Bivec2 i) = i *> e1e2
mvGeneralize (Rotor2 r i) = r *> 1 + i *> e1e2
mvGeneralize (Vec3 x y z) = x *> e1 + y *> e2 + z *> e3
mvGeneralize (Bivec3 i j k) = i *> e1e2 + j *> e1e3 + k *> e2e3
mvGeneralize (Trivec3 i) = i *> e1e2e3
mvGeneralize (Rotor3 r i j k) = r *> 1 + i *> e1e2 + j *> e1e3 + k *> e2e3

mvFromScalar :: Double -> Multivector
mvFromScalar 0 = Mv []
mvFromScalar x = Mv [B.scalar x]

getScalar :: Multivector -> Double
getScalar (Mv blades) 
  | Just blade <- find B.isScalar blades = coeff blade
  | otherwise					    			         = 0

-- * Basis vectors

-- | Generates a basis vector, e.g. e_1.
e_ :: Int -> Multivector
e_ n = Mv [B.e_ n]

e1, e2, e3, e1e2, e1e3, e2e3, e1e2e3 :: Multivector
e1 = e_ 1
e2 = e_ 2
e3 = e_ 3
e1e2 = e1*e2
e1e3 = e1*e3
e2e3 = e2*e3
e1e2e3 = e1e2*e3

-- | Pseudoscalar for 3-dimensional GA. Same as 'e1e2e3'.
i :: Multivector
i = e1e2e3

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
  | b1 == b2      = if s ~== 0 then xs else BasisBlade b1 s : xs
  | b1 < b2       = y : x : xs
  | otherwise     = x : bladeAdd y xs
  where s = s1 + s2

bladeSimplify :: [BasisBlade] -> [BasisBlade]
bladeSimplify = foldr bladeAdd []

-- * Comparisons

instance AEq Multivector where
  (===) = (==)
  (~==) = mvEqual

mvEqual :: Multivector -> Multivector -> Bool
(Mv bs1) `mvEqual` (Mv bs2) = and $ zipWith (~==) bs1 bs2 