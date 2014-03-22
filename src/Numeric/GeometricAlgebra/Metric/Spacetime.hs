-- | The spacetime metric consists of three euclidean basis vectors ('e1', 'e2', 'e3') and 'e0', representing time and obeying @e0 • e0 = -1@.

module Numeric.GeometricAlgebra.Metric.Spacetime 
  (e0, e1, e2, e3) where

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