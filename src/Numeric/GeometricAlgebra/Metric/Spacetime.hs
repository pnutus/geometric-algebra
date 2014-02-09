module Numeric.GeometricAlgebra.Metric.Spacetime where

import Numeric.GeometricAlgebra.Metric
import Numeric.GeometricAlgebra.Multivector (e_)

e1, e2, e3, e0 :: MetricMultivector
e1 = spacetimeBasisElement 1
e2 = spacetimeBasisElement 2
e3 = spacetimeBasisElement 3
e0 = spacetimeBasisElement 4

spacetimeBasisElement :: Int -> MetricMultivector
spacetimeBasisElement = MMv spacetimeMetric . e_

spacetimeMetric :: Metric
spacetimeMetric = DiagonalMetric ["e1", "e2", "e3", "e0"] [1, 1, 1, -1]