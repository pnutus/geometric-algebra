module Numeric.GeometricAlgebra.Metric where

import Numeric.LinearAlgebra
import Data.List (transpose)
import Data.Bits (bit, (.&.))
import Data.List (intercalate)

-- | A metric is a list of basis element names, a list of corresponding squares
-- and a transformation Matrix.
data Metric = EuclideanMetric
            | DiagonalMetric [String] [Square]
            | Metric [String] [Square] MetricMatrix
            deriving (Eq)
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


