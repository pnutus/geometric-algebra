-- | Geometric algebra module optimized for understanding, not efficiency.
-- In GA, both objects and operators are 'Multivector's that can be added
-- multiplied, exponentiated with expected results.
-- For more information on GA, see the
-- <http://en.wikipedia.org/wiki/Geometric_algebra Wikipedia article> on GA
-- (including referenced literature).

module Numeric.GeometricAlgebra 
  ( Multivector
  -- * Operators and operations
  , geo
  , (∧), out
  , (•), dot
  , (⎦), (⎣)
  , mvReverse
  , sandwich
  , rotorBetween
  , (<>), gradeProject
  -- * Constants
  , tupi, tau
  ) where

import Numeric.GeometricAlgebra.Multivector