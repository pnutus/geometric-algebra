-- | The conformal metric consists of three euclidean basis vectors ('e1', 'e2', 'e3'), the point at the origin ('no') and the point at infinity ('ni'), where:
--
-- prop> no `dot` e{i} = ni `dot` e{i} = 0
-- prop> no `dot` no = ni `dot` ni = 0
-- prop> no `dot` ni = -1

module Numeric.GeometricAlgebra.Metric.Conformal 
  (no, e1, e2, e3, ni) where

import Numeric.GeometricAlgebra.Metric
import Numeric.GeometricAlgebra.Multivector (Multivector, basisVector)

no, e1, e2, e3, ni :: Multivector
no = conformalBasisElement 1
e1 = conformalBasisElement 2
e2 = conformalBasisElement 3
e3 = conformalBasisElement 4
ni = conformalBasisElement 5

conformalBasisElement :: Int -> Multivector
conformalBasisElement = basisVector conformalMetric

conformalMetric :: Metric
conformalMetric = createMetric 
                  [  "o", "e1", "e2", "e3", "∞"]
                  [ [ 0 ,  0  ,  0  ,  0  , -1 ]
                  , [ 0 ,  1  ,  0  ,  0  ,  0 ]
                  , [ 0 ,  0  ,  1  ,  0  ,  0 ]
                  , [ 0 ,  0  ,  0  ,  1  ,  0 ]
                  , [-1 ,  0  ,  0  ,  0  ,  0 ]
                  ]

