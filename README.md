# PolyFunctors

A collection of (mostly pointless) exotic functor instances

```haskell
module PolyFunctorExample where

import Control.PolyFunctor

λ> quadmap (+1) (+2) (+3) (+4) (1,2,3,4)
(2,4,6,8)
```
