-- |
-- Module      : Control.PolyFunctor
--
-- Copyright   : (c) 2016 Owain Lewis
--
-- License     : BSD-style
-- Maintainer  : Owain Lewis <owain@owainlewis.com>
-- Stability   : experimental
-- Portability : GHC
--
{-# LANGUAGE TypeOperators #-}
module Control.PolyFunctor
  ( BiFunctor(..)
  , TriFunctor(..)
  , QuadFunctor(..)
  , Tri(..)
  , Quad(..)
  , CovariantFunctor
  , ContravariantFunctor
  , Profunctor
  ) where

class CovariantFunctor f where
    fmap :: (a -> b) -> f a -> f b

class ContravariantFunctor f where
    contramap :: (b -> a) -> f a -> f b

class Profunctor f where
    dimap :: (c -> a) -> (b -> d) -> f a b -> f c d

-----------------------------------------------------

class BiFunctor f where
    bimap :: (a -> c) -> (b -> d) -> f a b -> f c d

instance BiFunctor Either where
    bimap f _ (Left a)  = Left (f a)
    bimap _ g (Right a) = Right (g a)
    {-# INLINE bimap #-}

instance BiFunctor (,) where
    bimap f g (a, b) = (f a, g b)
    {-# INLINE bimap #-}

-----------------------------------------------------

class TriFunctor f where
  trimap :: (a -> x) ->
            (b -> y) ->
            (c -> z) ->
            (f a b c -> f x y z)

data Tri a b c =
    TriA a
  | TriB b
  | TriC c

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

-----------------------------------------------------

data Quad a b c d =
    QuadA a
  | QuadB b
  | QuadC c
  | QuadD d
  
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
