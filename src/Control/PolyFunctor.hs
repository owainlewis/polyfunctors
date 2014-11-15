{-# LANGUAGE TypeOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.PolyFunctor
-- Copyright   :  2014 Owain Lewis
-- License     :  public domain
--
-- Maintainer  :  owain@owainlewis.com
-- Stability   :  experimental
-- Portability :  portable
--
-- A collection of experimental polyfunctor instances you probably don't need
--
-----------------------------------------------------------------------------

module Control.PolyFunctor
  ( TriFunctor(..)
  , QuadFunctor(..)
  , Tri(..)
  , Quad(..)
  ) where

-----------------------------------------------------------------------------
-- TriFunctor

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

-----------------------------------------------------------------------------
-- Quad Functor
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
