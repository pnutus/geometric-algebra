module Numeric.GeometricAlgebra.Metric where

import Numeric.LinearAlgebra
import Numeric.GeometricAlgebra.Multivector
import Numeric.GeometricAlgebra.BasisBlade (BasisBlade(..))
import qualified Numeric.GeometricAlgebra.BasisBlade as B
import Data.List (transpose)
import Data.Bits (bit, (.&.))
import Data.List (intercalate)

-- | A metric is a list of basis element names, a list of corresponding squares
-- and a transformation Matrix.
data Metric = EuclideanMetric
            | DiagonalMetric [String] [Square]
            | Metric [String] [Square] MetricMatrix
            deriving (Eq, Show)
type Square = Double
type MetricMatrix = [[Double]]

getNames :: Metric -> [String]
getNames EuclideanMetric          = ["e" ++ show n | n <- [1..]]
getNames (DiagonalMetric names _) = names
getNames (Metric names _ _)       = names

-- | Creates a Metric object from a list of basis element names and a list of lists with the metric matrix.
createMetric :: [String] -> [[Double]] -> Metric
createMetric names metricMatrix 
  | not $ allEqual dimensions = error "Dimensions do not match.\n\
  \If there are n basis elements, the matrix has to be n by n."
  | not $ symmetric metricMatrix = error "Matrix has to be symmetric."
  | otherwise = Metric names (toList eigvals) (toLists eigvecs)
  where (eigvals, eigvecs) = eigSH $ fromLists metricMatrix
        dimensions = [length names, length metricMatrix] 
                      ++ map length metricMatrix
                      
allEqual :: Eq a => [a] -> Bool
allEqual xs = all (== head xs) (tail xs)

symmetric :: MetricMatrix -> Bool
symmetric m = transpose m == m

-- | Multivector that keeps track of the metric.
data MetricMultivector = MMv Metric Multivector

getMultivector :: MetricMultivector -> Multivector
getMultivector (MMv _ mv) = mv

getMetric :: MetricMultivector -> Metric
getMetric (MMv m _) = m

instance Num MetricMultivector where
  mmv@(MMv m mv) * mmv'@(MMv m' mv') 
    = sameMetricOrScalarElse mmv mmv' (MMv m $ metricProduct m B.geo mv mv')
  mmv@(MMv m mv) + mmv'@(MMv m' mv') 
    = sameMetricOrScalarElse mmv mmv' (MMv m $ mv + mv')
  negate        = sameAsMultivector negate
  abs           = sameAsMultivector abs
  signum        = sameAsMultivector signum
  fromInteger x = MMv EuclideanMetric $ fromInteger x

instance Fractional MetricMultivector where
	recip          = sameAsMultivector recip
	fromRational x = MMv EuclideanMetric $ fromRational x

-- * Instance helpers

sameAsMultivector :: (Multivector -> Multivector)
                  -> (MetricMultivector -> MetricMultivector)
sameAsMultivector f (MMv m mv) = MMv m $ f mv

sameMetricOrScalarElse :: MetricMultivector -> MetricMultivector -> a -> a
sameMetricOrScalarElse mmv mmv' expr 
  | differentMetric && not scalar = error "Metrics have to be the same"
  | otherwise                     = expr
  where differentMetric = getMetric mmv /= getMetric mmv'
        scalar = all (isScalar . getMultivector) [mmv, mmv']

-- * Metric products

metricProduct :: Metric
              -> (BasisBlade -> BasisBlade -> BasisBlade) 
              -> (Multivector -> Multivector -> Multivector)
metricProduct metric op mv1 mv2 = case metric of
  EuclideanMetric          -> mvProduct op mv1 mv2
  DiagonalMetric _ squares -> mvProduct metricOp mv1 mv2 
    where metricOp = (metricBladeProduct squares op)
  Metric _ squares m       -> fromEigenmetric m $ mvProduct metricOp mv1' mv2'
    where metricOp = (metricBladeProduct squares op)
          [mv1', mv2'] = map (toEigenmetric m) [mv1, mv2]  

metricBladeProduct :: [Square]
              -> (BasisBlade -> BasisBlade -> BasisBlade)
              -> (BasisBlade -> BasisBlade -> BasisBlade)
metricBladeProduct squares op b b' = factor B.*> (b `op` b') 
  where factor = metricFactor squares (basis b) (basis b')

metricFactor :: [Square] -> B.Bitmap -> B.Bitmap -> Double
metricFactor squares b b' = product $ bitFilter (b .&. b') squares

toEigenmetric :: MetricMatrix -> Multivector -> Multivector
toEigenmetric = metricTransform
  
fromEigenmetric :: MetricMatrix -> Multivector -> Multivector
fromEigenmetric m = metricTransform (transpose m)

metricTransform :: MetricMatrix -> Multivector -> Multivector
metricTransform m (Mv blades) = foldr step 0 blades
  where step b mv = mv + (Mv $ metricBladeTransform m b)

metricBladeTransform :: MetricMatrix -> BasisBlade -> [BasisBlade]
metricBladeTransform m b = foldr step [B.scalar $ coeff b] rows
  where step row bs = bladeSimplify [BasisBlade (bit i) c `B.out` b'  
                        | b' <- bs, (i, c) <- zip [0..] row, c /= 0 ]
        rows = bitFilter (basis b) m

-- * Show

instance Show MetricMultivector where
  show (MMv metric mv@(Mv bs)) = case metric of
    EuclideanMetric -> show mv 
    metric          -> printTerms [(c, printBasis b) | BasisBlade b c <- bs]
      where printBasis = printMetricBasis (getNames metric)
      
printMetricBasis names basis = intercalate "∧" $ bitFilter basis names