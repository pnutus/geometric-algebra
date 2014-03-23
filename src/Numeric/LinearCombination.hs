module Numeric.LinearCombination 
  ( LinearCombination(..)
  , Term(..)
  , multiplyUsing
  , operationUsing
  , basisChangeUsing
  , Numeric.LinearCombination.filter
  , find
  , elementsToList
  , scalarMult
  , zero
  , showUsing
  , simplify
  ) where

import GHC.Exts (sortWith)
import Data.AEq
import Numeric (showGFloat)
import qualified Data.List as L
import Prelude hiding (filter)

newtype LinearCombination coefficient element
  = LinComb [Term coefficient element]
  deriving (Eq)
data Term coefficient element = coefficient :* element
  deriving (Eq, Show)

instance (Num a) => Monad (Term a) where
  (c :* e) >>= f = (c * c') :* e'
     where (c' :* e') = f e
  return x = 1 :* x

instance (Num a, AEq a, Eq b, Ord b) => Num (LinearCombination a b) where
  (LinComb terms) + (LinComb terms') = LinComb $ foldr addTerm short long
    where [short, long] = sortWith length [terms, terms']
  negate = mapTerms (\(c :* e) -> (-c) :* e)
  (*) = error "Use the function `multiplyUsing` instead."
  abs = error "Absolute value not defined for LinearCombination."
  signum = error "Signum not defined for LinearCombination."
  fromInteger 0 = zero -- to allow sum
  fromInteger _ = error "Can't construct LinearCombination from integer."

mapTerms :: (Num a, Eq b, Ord b)
         => (Term a b -> Term a b)
         -> LinearCombination a b -> LinearCombination a b
mapTerms f (LinComb terms) = LinComb $ map f terms

-- | Multiplies two 'LinearCombination's given a rule to multiply two elements.
multiplyUsing :: (Num a, AEq a, Eq b, Ord b)
              => (b -> b -> Term a b)
              -> LinearCombination a b -> LinearCombination a b 
              -> LinearCombination a b
multiplyUsing op (LinComb xs) (LinComb ys) = simplify $ LinComb products
  where products = [(c1 * c2 * factor) :* e 
                    | (c1 :* e1) <- xs, (c2 :* e2) <- ys,
                    let factor :* e = e1 `op` e2,
                    factor /= 0] 
                    
operationUsing :: (Num a, AEq a, Eq b, Ord b) 
               => (b -> Term a b) -> LinearCombination a b
               -> LinearCombination a b
operationUsing op = simplify . (mapTerms (>>= op))

basisChangeUsing :: (Num a, AEq a, Eq b, Ord b) 
               => (b -> LinearCombination a b) -> LinearCombination a b
               -> LinearCombination a b
basisChangeUsing op (LinComb terms) = sum $ map transform terms
  where transform (c :* e) = c `scalarMult` op e 

filter :: (Num a, AEq a, Eq b, Ord b) 
          => (b -> Bool) -> LinearCombination a b
          -> LinearCombination a b
filter p (LinComb terms) = LinComb $ L.filter (p . element) terms

find :: (Num a, AEq a, Eq b, Ord b) 
          => (b -> Bool) -> LinearCombination a b
          -> Maybe (Term a b)
find p (LinComb terms) = L.find (p . element) terms

elementsToList :: LinearCombination a b -> [b]
elementsToList (LinComb terms) = map element terms

element :: Term a b -> b
element (c :* e) = e


-- | Adds a Term to a list of Terms.
addTerm :: (Num a, AEq a, Eq b, Ord b)
        => Term a b -> [Term a b] 
        -> [Term a b]
addTerm y [] = [y]
addTerm y@(c1 :* e1) (x@(c2 :* e2):xs)
  | e1 == e2      = if c ~== 0 then xs else (c :* e1) : xs
  | e1 < e2       = y : x : xs
  | otherwise     = x : addTerm y xs
  where c = c1 + c2

-- | Simplifies a LinearCombination so that each element only appears once by
-- adding the coefficients of like elements.
simplify :: (Num a, AEq a, Eq b, Ord b)
        => LinearCombination a b 
        -> LinearCombination a b
simplify (LinComb xs) = LinComb $ foldr addTerm [] xs

-- | Multiplication by a scalar
scalarMult :: (Num a, Eq b, Ord b)
        => a -> LinearCombination a b 
        -> LinearCombination a b
scalarMult x = mapTerms (\(c :* e) -> (x * c) :* e)

zero :: LinearCombination a b
zero = LinComb []

-- * Printing

instance (Show a, Num a, AEq a, Ord a, Show b) 
      => Show (LinearCombination a b) where
  show = showUsing show

showUsing :: (Show a, Num a, AEq a, Ord a) 
          => (b -> String) -> LinearCombination a b 
          -> String
showUsing _ (LinComb []) = "0"
showUsing showElement (LinComb terms) = concat $ first : rest
  where first = showTerm True (head terms)
        rest  = map (showTerm False) (tail terms)
        showTerm = showTermUsing showElement
        
showTermUsing :: (Show a, Num a, AEq a, Ord a)
              => (b -> String) -> Bool -> Term a b 
              -> String
showTermUsing showElement first (c :* e) = sign ++ number ++ eString
  where sign | first      = if c < 0 then "-" else ""
             | otherwise  = if c < 0 then " - " else " + "
        number = if absc ~== 1 && eString /= "" then "" else absString
        absString = show absc--GFloat (Just 3) absc ""
        absc      = abs c
        eString = showElement e

instance (Num a, AEq a, Eq b) => AEq (LinearCombination a b) where
  (===) = (==)
  (LinComb terms) ~== (LinComb terms') = and $ zipWith (~==) terms terms'
    
instance (Num a, AEq a, Eq b) => AEq (Term a b) where
  (===) = (==)
  (c :* e) ~== (c' :* e') = (c ~== c') && (e == e')