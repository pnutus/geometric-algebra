-- | Geometric algebra module optimized for understanding, not efficiency.
-- In GA, both objects and operators are 'Multivector's that can be added
-- multiplied, exponentiated with expected results.
-- For more information on GA, see the
-- <http://en.wikipedia.org/wiki/Geometric_algebra Wikipedia article> on GA
-- (including the literature referenced there).
--
-- To start calculating, you need some basis vectors. 
-- Import /one/ of the metric modules ('Euclidean', 'Spacetime', 'Conformal')
--
-- > import Numeric.GeometricAlgebra.Metric.Euclidean
--
-- and you're all set!
--
-- >>> (e1 + e2)*e2
-- 1.0 + e1∧e2

module Numeric.GeometricAlgebra 
  ( Multivector
  -- * Operators and operations
  , geo
  , (∧), out
  , (•), dot
  , (⎦), (⎣)
  , (*>)
  , mvReverse
  , sandwich
  , rotorBetween
  , (<>), gradeProject
  -- * Constants
  , tupi, tau
  ) where

import Numeric.GeometricAlgebra.Multivector