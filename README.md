Geometric Algebra
=================

A library for Geometric Algebra calculations in Haskell.

Installation
------------

The package is not yet on Hackage, so you need to download from here and install:

```
git clone git@github.com:pnutus/geometric-algebra.git
cd geometric-algebra
cabal install
```

Usage
-----

Start by importing the package and a Metric:

```
import Numeric.GeometricAlgebra
import Numeric.GeometricAlgebra.Metric.Euclidean
```

Now you can start doing calculations!

```
>>> (e1 + e2)*e3
e1∧e3 + e2∧e3
```