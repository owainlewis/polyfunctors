# PolyFunctors

Functor-like type classes for multi-parameter types in Haskell.

## What are PolyFunctors?

Haskell's standard `Functor` only works with types that have one type parameter (like `Maybe a` or `[a]`). But what about types with multiple parameters like `Either a b` or `(a, b, c)`?

PolyFunctors generalize the functor pattern to these multi-parameter types:

- **BiFunctor** lets you map over both sides of `Either` or a 2-tuple
- **TriFunctor** extends this to 3-tuples
- **QuadFunctor** / **PentaFunctor** go even further

This library also provides the corresponding `Foldable` and `Traversable` variants, plus utilities like `Swap`, `Rotate3`, and `Validation` (an `Either` that accumulates errors).

## Why use this?

- **Transform tuples uniformly** — apply different functions to each position in one call
- **Work with sum types** — `Tri`, `Quad`, `Penta` are n-way alternatives to `Either`
- **Accumulate validation errors** — `Validation` collects all failures instead of short-circuiting
- **Compose functions flexibly** — `Profunctor` lets you map over both input and output

## Installation

```cabal
build-depends: polyfunctors
```

## Quick Example

```haskell
import Control.PolyFunctor

-- Map over all positions of n-tuples
bimap   (+1) (+2)           (1, 2)       -- (2, 4)
trimap  (+1) (+2) (+3)      (1, 2, 3)    -- (2, 4, 6)
quadmap (+1) (+2) (+3) (+4) (1, 2, 3, 4) -- (2, 4, 6, 8)

-- Validation accumulates errors (unlike Either)
let v1 = validate "name empty" (not . null) ""
    v2 = validate "age negative" (> 0) (-1)
in (,) <$> v1 <*> v2  -- Failure "name emptyage negative"
```

## Type Classes

| Class | Description |
|-------|-------------|
| `BiFunctor` | Map over 2-parameter types (`Either`, tuples) |
| `TriFunctor` | Map over 3-parameter types |
| `QuadFunctor` | Map over 4-parameter types |
| `PentaFunctor` | Map over 5-parameter types |
| `Profunctor` | Contravariant in first arg, covariant in second |
| `BiFoldable` / `TriFoldable` / `QuadFoldable` | Fold variants |
| `BiTraversable` / `TriTraversable` / `QuadTraversable` | Traverse variants |
| `Swap` | Swap two type parameters |
| `Rotate3` | Rotate three type parameters |

## Data Types

- **`Tri a b c`** — 3-way sum type (`TriA a | TriB b | TriC c`)
- **`Quad a b c d`** — 4-way sum type
- **`Penta a b c d e`** — 5-way sum type
- **`Validation e a`** — Like `Either` but accumulates errors via `Semigroup`

## Helpers

```haskell
first  (+1) (1, 2)        -- (2, 2)
second (+1) (1, 2)        -- (1, 3)
swap   (Left 1)           -- Right 1
rotate3L (1, 2, 3)        -- (2, 3, 1)
lmap (*2) (+10) $ 5       -- 20  (pre-compose)
rmap (*2) (+10) $ 5       -- 30  (post-compose)
```

## License

MIT
