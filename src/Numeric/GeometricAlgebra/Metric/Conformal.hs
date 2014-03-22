module Numeric.GeometricAlgebra.Metric.Conformal where

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
