module Numeric.GeometricAlgebra.Metric.Spacetime where

import Numeric.GeometricAlgebra.Metric
import Numeric.GeometricAlgebra.Multivector (Multivector, basisVector)

e1, e2, e3, e0 :: Multivector
e1 = spacetimeBasisElement 1
e2 = spacetimeBasisElement 2
e3 = spacetimeBasisElement 3
e0 = spacetimeBasisElement 4

spacetimeBasisElement :: Int -> Multivector
spacetimeBasisElement = basisVector spacetimeMetric

spacetimeMetric :: Metric
spacetimeMetric = DiagonalMetric ["e1", "e2", "e3", "e0"] [1, 1, 1, -1]