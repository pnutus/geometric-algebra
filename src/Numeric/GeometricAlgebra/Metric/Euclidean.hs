-- | The euclidean metric consists of three euclidean basis vectors ('e1', 'e2', 'e3').

module Numeric.GeometricAlgebra.Metric.Euclidean 
  (e1, e2, e3) where

import Numeric.GeometricAlgebra.Metric
import Numeric.GeometricAlgebra.Multivector (Multivector, basisVector)

e1, e2, e3, e1e2, e1e3, e2e3, e1e2e3 :: Multivector
e1 = euclideanBasisElement 1
e2 = euclideanBasisElement 2
e3 = euclideanBasisElement 3
e1e2 = e1*e2
e1e3 = e1*e3
e2e3 = e2*e3
e1e2e3 = e1e2*e3

-- Pseudoscalar for 3-dimensional GA. Same as 'e1e2e3'.
i :: Multivector
i = e1e2e3

euclideanBasisElement :: Int -> Multivector
euclideanBasisElement = basisVector EuclideanMetric