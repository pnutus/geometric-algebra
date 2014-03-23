Geometric Algebra
=================

A library for Geometric Algebra calculations in Haskell.

Installation
------------

Just use Cabal:

```
cd ~/path/to/geometric-algebra
cabal install
```

The package is not yet on Hackage, so you need to download from here and install.

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