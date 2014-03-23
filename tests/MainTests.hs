module Main where

import qualified MultivectorTests as M
import qualified BasisBladeTests as B
import Test.Framework
main = defaultMain [B.tests, M.tests]