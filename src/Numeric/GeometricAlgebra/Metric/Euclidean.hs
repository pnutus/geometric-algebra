module Numeric.GeometricAlgebra.Metric.Euclidean where

import Numeric.GeometricAlgebra.Metric
import Numeric.GeometricAlgebra.Multivector (e_)

e1, e2, e3, e1e2, e1e3, e2e3, e1e2e3 :: MetricMultivector
e1 = euclideanBasisElement 1
e2 = euclideanBasisElement 2
e3 = euclideanBasisElement 3
e1e2 = e1*e2
e1e3 = e1*e3
e2e3 = e2*e3
e1e2e3 = e1e2*e3

-- | Pseudoscalar for 3-dimensional GA. Same as 'e1e2e3'.
i :: MetricMultivector
i = e1e2e3

euclideanBasisElement :: Int -> MetricMultivector
euclideanBasisElement = MMv EuclideanMetric . e_