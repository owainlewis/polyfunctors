-- | ---------------------------------------------
-- Module      : Control.PolyFunctor
--
-- Copyright   : (c) 2016 Owain Lewis
--
-- License     : BSD-style
-- Maintainer  : Owain Lewis <owain@owainlewis.com>
-- Stability   : experimental
-- Portability : GHC
--
-- A collection of exotic functor instances for types with multiple
-- type parameters, extending the standard Functor to work with
-- bifunctors, trifunctors, and beyond.
-- | ---------------------------------------------
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Control.PolyFunctor
  ( -- * Covariant and Contravariant Functors
    CovariantFunctor(..)
  , ContravariantFunctor(..)
  , Profunctor(..)
  , lmap
  , rmap
    -- * BiFunctor
  , BiFunctor(..)
  , first
  , second
    -- * TriFunctor
  , TriFunctor(..)
  , Tri(..)
  , mapTri
    -- * QuadFunctor
  , QuadFunctor(..)
  , Quad(..)
  , mapQuad
    -- * PentaFunctor
  , PentaFunctor(..)
  , Penta(..)
  , mapPenta
    -- * Foldable variants
  , BiFoldable(..)
  , TriFoldable(..)
  , QuadFoldable(..)
    -- * Traversable variants
  , BiTraversable(..)
  , TriTraversable(..)
  , QuadTraversable(..)
    -- * Swap operations
  , Swap(..)
  , Rotate3(..)
    -- * Validation (accumulating Either)
  , Validation(..)
  , validationToEither
  , eitherToValidation
  , validate
  ) where

import Data.Monoid ((<>))

class CovariantFunctor f where
    fmap :: (a -> b) -> f a -> f b

class ContravariantFunctor f where
    contramap :: (b -> a) -> f a -> f b

class Profunctor f where
    dimap :: (c -> a) -> (b -> d) -> f a b -> f c d

class BiFunctor f where
    bimap :: (a -> c) -> (b -> d) -> f a b -> f c d

instance BiFunctor Either where
    bimap f _ (Left a)  = Left (f a)
    bimap _ g (Right a) = Right (g a)
    {-# INLINE bimap #-}

instance BiFunctor (,) where
    bimap f g (a, b) = (f a, g b)
    {-# INLINE bimap #-}

-- | Map over the first argument only
first :: BiFunctor f => (a -> c) -> f a b -> f c b
first f = bimap f id
{-# INLINE first #-}

-- | Map over the second argument only
second :: BiFunctor f => (b -> d) -> f a b -> f a d
second = bimap id
{-# INLINE second #-}

class TriFunctor f where
  trimap :: (a -> x) ->
            (b -> y) ->
            (c -> z) ->
            (f a b c -> f x y z)

data Tri a b c =
    TriA a
  | TriB b
  | TriC c
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance TriFunctor (,,) where
    trimap f g h (x, y, z) = ((f x), (g y), (h z))
    {-# INLINE trimap #-}

mapTri :: TriFunctor f => (a -> x) -> (b -> y) -> (c -> z) -> [f a b c] -> [f x y z]
mapTri f g h = map $ trimap f g h

instance TriFunctor Tri where
    trimap f _ _ (TriA x) = TriA (f x)
    trimap _ g _ (TriB y) = TriB (g y)
    trimap _ _ h (TriC z) = TriC (h z)
    {-# INLINE trimap #-}
    
data Quad a b c d =
    QuadA a
  | QuadB b
  | QuadC c
  | QuadD d
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
  
class QuadFunctor f where
    quadmap :: (a -> a') ->
               (b -> b') ->
               (c -> c') ->
               (d -> d') ->
               (f a b c d -> f a' b' c' d')

instance QuadFunctor (,,,) where
    quadmap f g h i (a, b, c, d) = ((f a), (g b), (h c), (i d))
    {-# INLINE quadmap #-}

instance QuadFunctor Quad where
    quadmap f _ _ _ (QuadA a) = QuadA (f a)
    quadmap _ g _ _ (QuadB b) = QuadB (g b)
    quadmap _ _ h _ (QuadC c) = QuadC (h c)
    quadmap _ _ _ i (QuadD d) = QuadD (i d)
    {-# INLINE quadmap #-}

mapQuad
  :: QuadFunctor f =>
        (a -> a')
     -> (b -> b')
     -> (c -> c')
     -> (d -> d')
     -> [f a b c d]
     -> [f a' b' c' d']
mapQuad f g h i = map $ quadmap f g h i

-- -----------------------------------------------------------------------------
-- PentaFunctor - 5 parameter types
-- -----------------------------------------------------------------------------

data Penta a b c d e =
    PentaA a
  | PentaB b
  | PentaC c
  | PentaD d
  | PentaE e
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

class PentaFunctor f where
    pentamap :: (a -> a')
             -> (b -> b')
             -> (c -> c')
             -> (d -> d')
             -> (e -> e')
             -> (f a b c d e -> f a' b' c' d' e')

instance PentaFunctor (,,,,) where
    pentamap f g h i j (a, b, c, d, e) = (f a, g b, h c, i d, j e)
    {-# INLINE pentamap #-}

instance PentaFunctor Penta where
    pentamap f _ _ _ _ (PentaA a) = PentaA (f a)
    pentamap _ g _ _ _ (PentaB b) = PentaB (g b)
    pentamap _ _ h _ _ (PentaC c) = PentaC (h c)
    pentamap _ _ _ i _ (PentaD d) = PentaD (i d)
    pentamap _ _ _ _ j (PentaE e) = PentaE (j e)
    {-# INLINE pentamap #-}

mapPenta
  :: PentaFunctor f
  => (a -> a')
  -> (b -> b')
  -> (c -> c')
  -> (d -> d')
  -> (e -> e')
  -> [f a b c d e]
  -> [f a' b' c' d' e']
mapPenta f g h i j = map $ pentamap f g h i j

-- -----------------------------------------------------------------------------
-- Profunctor instance for functions
-- -----------------------------------------------------------------------------

instance Profunctor (->) where
    dimap f g h = g . h . f
    {-# INLINE dimap #-}

-- | Map over the input of a function (contravariant position)
lmap :: Profunctor f => (c -> a) -> f a b -> f c b
lmap f = dimap f id
{-# INLINE lmap #-}

-- | Map over the output of a function (covariant position)
rmap :: Profunctor f => (b -> d) -> f a b -> f a d
rmap = dimap id
{-# INLINE rmap #-}

-- -----------------------------------------------------------------------------
-- BiFoldable, TriFoldable, QuadFoldable
-- -----------------------------------------------------------------------------

class BiFoldable f where
    bifoldMap :: Monoid m => (a -> m) -> (b -> m) -> f a b -> m

instance BiFoldable Either where
    bifoldMap f _ (Left a)  = f a
    bifoldMap _ g (Right b) = g b
    {-# INLINE bifoldMap #-}

instance BiFoldable (,) where
    bifoldMap f g (a, b) = f a <> g b
    {-# INLINE bifoldMap #-}

class TriFoldable f where
    trifoldMap :: Monoid m => (a -> m) -> (b -> m) -> (c -> m) -> f a b c -> m

instance TriFoldable (,,) where
    trifoldMap f g h (a, b, c) = f a <> g b <> h c
    {-# INLINE trifoldMap #-}

instance TriFoldable Tri where
    trifoldMap f _ _ (TriA a) = f a
    trifoldMap _ g _ (TriB b) = g b
    trifoldMap _ _ h (TriC c) = h c
    {-# INLINE trifoldMap #-}

class QuadFoldable f where
    quadfoldMap :: Monoid m => (a -> m) -> (b -> m) -> (c -> m) -> (d -> m) -> f a b c d -> m

instance QuadFoldable (,,,) where
    quadfoldMap f g h i (a, b, c, d) = f a <> g b <> h c <> i d
    {-# INLINE quadfoldMap #-}

instance QuadFoldable Quad where
    quadfoldMap f _ _ _ (QuadA a) = f a
    quadfoldMap _ g _ _ (QuadB b) = g b
    quadfoldMap _ _ h _ (QuadC c) = h c
    quadfoldMap _ _ _ i (QuadD d) = i d
    {-# INLINE quadfoldMap #-}

-- -----------------------------------------------------------------------------
-- BiTraversable, TriTraversable, QuadTraversable
-- -----------------------------------------------------------------------------

class (BiFunctor t, BiFoldable t) => BiTraversable t where
    bitraverse :: Applicative f => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)

instance BiTraversable Either where
    bitraverse f _ (Left a)  = Left <$> f a
    bitraverse _ g (Right b) = Right <$> g b
    {-# INLINE bitraverse #-}

instance BiTraversable (,) where
    bitraverse f g (a, b) = (,) <$> f a <*> g b
    {-# INLINE bitraverse #-}

class (TriFunctor t, TriFoldable t) => TriTraversable t where
    tritraverse :: Applicative f => (a -> f x) -> (b -> f y) -> (c -> f z) -> t a b c -> f (t x y z)

instance TriTraversable (,,) where
    tritraverse f g h (a, b, c) = (,,) <$> f a <*> g b <*> h c
    {-# INLINE tritraverse #-}

instance TriTraversable Tri where
    tritraverse f _ _ (TriA a) = TriA <$> f a
    tritraverse _ g _ (TriB b) = TriB <$> g b
    tritraverse _ _ h (TriC c) = TriC <$> h c
    {-# INLINE tritraverse #-}

class (QuadFunctor t, QuadFoldable t) => QuadTraversable t where
    quadtraverse :: Applicative f
                 => (a -> f a')
                 -> (b -> f b')
                 -> (c -> f c')
                 -> (d -> f d')
                 -> t a b c d
                 -> f (t a' b' c' d')

instance QuadTraversable (,,,) where
    quadtraverse f g h i (a, b, c, d) = (,,,) <$> f a <*> g b <*> h c <*> i d
    {-# INLINE quadtraverse #-}

instance QuadTraversable Quad where
    quadtraverse f _ _ _ (QuadA a) = QuadA <$> f a
    quadtraverse _ g _ _ (QuadB b) = QuadB <$> g b
    quadtraverse _ _ h _ (QuadC c) = QuadC <$> h c
    quadtraverse _ _ _ i (QuadD d) = QuadD <$> i d
    {-# INLINE quadtraverse #-}

-- -----------------------------------------------------------------------------
-- Swap and Rotate operations
-- -----------------------------------------------------------------------------

-- | Types that support swapping their two type parameters
class Swap f where
    swap :: f a b -> f b a

instance Swap Either where
    swap (Left a)  = Right a
    swap (Right b) = Left b
    {-# INLINE swap #-}

instance Swap (,) where
    swap (a, b) = (b, a)
    {-# INLINE swap #-}

-- | Types that support rotation of their three type parameters
class Rotate3 f where
    -- | Rotate left: (a, b, c) -> (b, c, a)
    rotate3L :: f a b c -> f b c a
    -- | Rotate right: (a, b, c) -> (c, a, b)
    rotate3R :: f a b c -> f c a b

instance Rotate3 (,,) where
    rotate3L (a, b, c) = (b, c, a)
    rotate3R (a, b, c) = (c, a, b)
    {-# INLINE rotate3L #-}
    {-# INLINE rotate3R #-}

instance Rotate3 Tri where
    rotate3L (TriA a) = TriC a
    rotate3L (TriB b) = TriA b
    rotate3L (TriC c) = TriB c
    rotate3R (TriA a) = TriB a
    rotate3R (TriB b) = TriC b
    rotate3R (TriC c) = TriA c
    {-# INLINE rotate3L #-}
    {-# INLINE rotate3R #-}

-- -----------------------------------------------------------------------------
-- Validation - Either with accumulating errors
-- -----------------------------------------------------------------------------

-- | Like Either, but accumulates errors using a Semigroup
data Validation e a =
    Failure e
  | Success a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance BiFunctor Validation where
    bimap f _ (Failure e) = Failure (f e)
    bimap _ g (Success a) = Success (g a)
    {-# INLINE bimap #-}

instance BiFoldable Validation where
    bifoldMap f _ (Failure e) = f e
    bifoldMap _ g (Success a) = g a
    {-# INLINE bifoldMap #-}

instance BiTraversable Validation where
    bitraverse f _ (Failure e) = Failure <$> f e
    bitraverse _ g (Success a) = Success <$> g a
    {-# INLINE bitraverse #-}

instance Swap Validation where
    swap (Failure e) = Success e
    swap (Success a) = Failure a
    {-# INLINE swap #-}

-- | Applicative that accumulates errors
instance Semigroup e => Applicative (Validation e) where
    pure = Success
    Failure e1 <*> Failure e2 = Failure (e1 <> e2)
    Failure e  <*> _          = Failure e
    _          <*> Failure e  = Failure e
    Success f  <*> Success a  = Success (f a)
    {-# INLINE pure #-}
    {-# INLINE (<*>) #-}

-- | Convert Validation to Either
validationToEither :: Validation e a -> Either e a
validationToEither (Failure e) = Left e
validationToEither (Success a) = Right a
{-# INLINE validationToEither #-}

-- | Convert Either to Validation
eitherToValidation :: Either e a -> Validation e a
eitherToValidation (Left e)  = Failure e
eitherToValidation (Right a) = Success a
{-# INLINE eitherToValidation #-}

-- | Create a Validation from a predicate
validate :: e -> (a -> Bool) -> a -> Validation e a
validate e p a = if p a then Success a else Failure e
{-# INLINE validate #-}
