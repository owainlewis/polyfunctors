{-# LANGUAGE TypeOperators #-}

module Control.PolyFunctor
  ( ) where

class TriFunctor f where
  trimap :: (a -> x) ->
            (b -> y) ->
            (c -> z) ->
            (f a b c -> f x y z)

data Tri a b c =
  TriA a |
  TriB b |
  TriC c

instance TriFunctor (,,) where
  trimap f g h (x, y, z) = ((f x), (g y), (h z))

instance TriFunctor Tri where
    trimap f _ _ (TriA x) = (TriA (f x))
    trimap _ g _ (TriB y) = (TriB (g y))
    trimap _ _ h (TriC z) = (TriC (h z))

class QuadFunctor f where
    quadmap :: (a -> a') -> (b -> b') -> (c -> c') -> (d -> d') -> (f a b c d -> f a' b' c' d')

instance QuadFunctor (,,,) where
    quadmap f g h i (a, b, c, d) = ((f a), (g b), (h c), (i d))
