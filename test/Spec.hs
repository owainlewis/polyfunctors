{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Test.Hspec
import Test.QuickCheck

import Control.PolyFunctor

main :: IO ()
main = hspec $ do
  describe "BiFunctor" $ do
    describe "bimap" $ do
      it "maps over both components of a tuple" $ do
        bimap (+1) (*2) (1, 3) `shouldBe` (2, 6)

      it "maps over Left of Either" $ do
        bimap (+1) (*2) (Left 5 :: Either Int Int) `shouldBe` Left 6

      it "maps over Right of Either" $ do
        bimap (+1) (*2) (Right 5 :: Either Int Int) `shouldBe` Right 10

    describe "first" $ do
      it "maps over the first component only" $ do
        first (+1) (1, 2) `shouldBe` (2, 2)

      it "maps over Left only" $ do
        first (+1) (Left 5 :: Either Int Int) `shouldBe` Left 6

      it "leaves Right unchanged" $ do
        first (+1) (Right 5 :: Either Int Int) `shouldBe` Right 5

    describe "second" $ do
      it "maps over the second component only" $ do
        second (*2) (1, 3) `shouldBe` (1, 6)

      it "leaves Left unchanged" $ do
        second (*2) (Left 5 :: Either Int Int) `shouldBe` Left 5

      it "maps over Right only" $ do
        second (*2) (Right 5 :: Either Int Int) `shouldBe` Right 10

  describe "TriFunctor" $ do
    describe "trimap" $ do
      it "maps over all three components of a 3-tuple" $ do
        trimap (+1) (*2) show (1, 2, 3) `shouldBe` (2, 4, "3")

      it "maps over TriA" $ do
        trimap (+1) (*2) show (TriA 5 :: Tri Int Int Int) `shouldBe` TriA 6

      it "maps over TriB" $ do
        trimap (+1) (*2) show (TriB 5 :: Tri Int Int Int) `shouldBe` TriB 10

      it "maps over TriC" $ do
        trimap (+1) (*2) show (TriC 5 :: Tri Int Int Int) `shouldBe` TriC "5"

    describe "mapTri" $ do
      it "maps trimap over a list" $ do
        mapTri (+1) (*2) show [(1,2,3), (4,5,6)] `shouldBe` [(2,4,"3"), (5,10,"6")]

  describe "QuadFunctor" $ do
    describe "quadmap" $ do
      it "maps over all four components of a 4-tuple" $ do
        quadmap (+1) (*2) (+10) (*10) (1, 2, 3, 4) `shouldBe` (2, 4, 13, 40)

      it "maps over QuadA" $ do
        quadmap (+1) (*2) (+10) (*10) (QuadA 5 :: Quad Int Int Int Int) `shouldBe` QuadA 6

      it "maps over QuadB" $ do
        quadmap (+1) (*2) (+10) (*10) (QuadB 5 :: Quad Int Int Int Int) `shouldBe` QuadB 10

      it "maps over QuadC" $ do
        quadmap (+1) (*2) (+10) (*10) (QuadC 5 :: Quad Int Int Int Int) `shouldBe` QuadC 15

      it "maps over QuadD" $ do
        quadmap (+1) (*2) (+10) (*10) (QuadD 5 :: Quad Int Int Int Int) `shouldBe` QuadD 50

  describe "PentaFunctor" $ do
    describe "pentamap" $ do
      it "maps over all five components of a 5-tuple" $ do
        pentamap (+1) (+2) (+3) (+4) (+5) (1,2,3,4,5) `shouldBe` (2,4,6,8,10)

      it "maps over PentaA" $ do
        pentamap (+1) (+2) (+3) (+4) (+5) (PentaA 10 :: Penta Int Int Int Int Int)
          `shouldBe` PentaA 11

      it "maps over PentaE" $ do
        pentamap (+1) (+2) (+3) (+4) (+5) (PentaE 10 :: Penta Int Int Int Int Int)
          `shouldBe` PentaE 15

  describe "Profunctor" $ do
    describe "dimap" $ do
      it "maps both input and output of a function" $ do
        let f = dimap (*2) (+1) (+10) :: Int -> Int
        f 5 `shouldBe` 21  -- (*2) 5 = 10, (+10) 10 = 20, (+1) 20 = 21

    describe "lmap" $ do
      it "maps over the input of a function" $ do
        let f = lmap (*2) (+10) :: Int -> Int
        f 5 `shouldBe` 20  -- (*2) 5 = 10, (+10) 10 = 20

    describe "rmap" $ do
      it "maps over the output of a function" $ do
        let f = rmap (*2) (+10) :: Int -> Int
        f 5 `shouldBe` 30  -- (+10) 5 = 15, (*2) 15 = 30

  describe "BiFoldable" $ do
    describe "bifoldMap" $ do
      it "folds both components of a tuple" $ do
        bifoldMap show show (1 :: Int, 2 :: Int) `shouldBe` "12"

      it "folds Left of Either" $ do
        bifoldMap show show (Left 1 :: Either Int Int) `shouldBe` "1"

      it "folds Right of Either" $ do
        bifoldMap show show (Right 2 :: Either Int Int) `shouldBe` "2"

  describe "TriFoldable" $ do
    describe "trifoldMap" $ do
      it "folds all three components of a 3-tuple" $ do
        trifoldMap show show show (1 :: Int, 2 :: Int, 3 :: Int) `shouldBe` "123"

      it "folds TriA" $ do
        trifoldMap show show show (TriA 1 :: Tri Int Int Int) `shouldBe` "1"

      it "folds TriB" $ do
        trifoldMap show show show (TriB 2 :: Tri Int Int Int) `shouldBe` "2"

  describe "QuadFoldable" $ do
    describe "quadfoldMap" $ do
      it "folds all four components of a 4-tuple" $ do
        quadfoldMap show show show show (1 :: Int, 2 :: Int, 3 :: Int, 4 :: Int) `shouldBe` "1234"

      it "folds QuadC" $ do
        quadfoldMap show show show show (QuadC 3 :: Quad Int Int Int Int) `shouldBe` "3"

  describe "BiTraversable" $ do
    describe "bitraverse" $ do
      it "traverses both components of a tuple" $ do
        bitraverse Just Just (1 :: Int, 2 :: Int) `shouldBe` Just (1, 2)

      it "traverses Either" $ do
        bitraverse Just Just (Right 2 :: Either Int Int) `shouldBe` Just (Right 2)

  describe "TriTraversable" $ do
    describe "tritraverse" $ do
      it "traverses all three components of a 3-tuple" $ do
        tritraverse Just Just Just (1 :: Int, 2 :: Int, 3 :: Int) `shouldBe` Just (1, 2, 3)

      it "traverses Tri" $ do
        tritraverse Just Just Just (TriB 2 :: Tri Int Int Int) `shouldBe` Just (TriB 2)

  describe "QuadTraversable" $ do
    describe "quadtraverse" $ do
      it "traverses all four components of a 4-tuple" $ do
        quadtraverse Just Just Just Just (1 :: Int, 2 :: Int, 3 :: Int, 4 :: Int)
          `shouldBe` Just (1, 2, 3, 4)

  describe "Swap" $ do
    describe "swap" $ do
      it "swaps tuple components" $ do
        swap (1 :: Int, "hello") `shouldBe` ("hello", 1)

      it "swaps Left to Right" $ do
        swap (Left 1 :: Either Int String) `shouldBe` Right 1

      it "swaps Right to Left" $ do
        swap (Right "hello" :: Either Int String) `shouldBe` Left "hello"

      it "swaps Failure to Success" $ do
        swap (Failure "error" :: Validation String Int) `shouldBe` Success "error"

      it "swaps Success to Failure" $ do
        swap (Success 42 :: Validation String Int) `shouldBe` Failure 42

  describe "Rotate3" $ do
    describe "rotate3L" $ do
      it "rotates 3-tuple left" $ do
        rotate3L (1 :: Int, 2 :: Int, 3 :: Int) `shouldBe` (2, 3, 1)

      it "rotates Tri left" $ do
        rotate3L (TriA 1 :: Tri Int Int Int) `shouldBe` TriC 1
        rotate3L (TriB 2 :: Tri Int Int Int) `shouldBe` TriA 2
        rotate3L (TriC 3 :: Tri Int Int Int) `shouldBe` TriB 3

    describe "rotate3R" $ do
      it "rotates 3-tuple right" $ do
        rotate3R (1 :: Int, 2 :: Int, 3 :: Int) `shouldBe` (3, 1, 2)

      it "rotates Tri right" $ do
        rotate3R (TriA 1 :: Tri Int Int Int) `shouldBe` TriB 1
        rotate3R (TriB 2 :: Tri Int Int Int) `shouldBe` TriC 2
        rotate3R (TriC 3 :: Tri Int Int Int) `shouldBe` TriA 3

    describe "rotation laws" $ do
      it "rotate3L . rotate3R = id for tuples" $ property $
        \(a :: Int) (b :: Int) (c :: Int) ->
          rotate3L (rotate3R (a, b, c)) == (a, b, c)

      it "rotate3R . rotate3L = id for tuples" $ property $
        \(a :: Int) (b :: Int) (c :: Int) ->
          rotate3R (rotate3L (a, b, c)) == (a, b, c)

  describe "Validation" $ do
    describe "constructors" $ do
      it "creates Failure" $ do
        (Failure "error" :: Validation String Int) `shouldBe` Failure "error"

      it "creates Success" $ do
        (Success 42 :: Validation String Int) `shouldBe` Success 42

    describe "Functor instance" $ do
      it "maps over Success" $ do
        fmap (+1) (Success 41 :: Validation String Int) `shouldBe` Success 42

      it "leaves Failure unchanged" $ do
        fmap (+1) (Failure "error" :: Validation String Int) `shouldBe` Failure "error"

    describe "Applicative instance" $ do
      it "applies Success to Success" $ do
        (Success (+1) <*> Success 41 :: Validation String Int) `shouldBe` Success 42

      it "accumulates errors" $ do
        let v1 = Failure "error1" :: Validation String Int
            v2 = Failure "error2" :: Validation String Int
        ((,) <$> v1 <*> v2) `shouldBe` Failure "error1error2"

      it "returns Failure when first is Failure" $ do
        let v1 = Failure "error" :: Validation String Int
            v2 = Success 42 :: Validation String Int
        ((,) <$> v1 <*> v2) `shouldBe` Failure "error"

      it "returns Failure when second is Failure" $ do
        let v1 = Success 1 :: Validation String Int
            v2 = Failure "error" :: Validation String Int
        ((,) <$> v1 <*> v2) `shouldBe` Failure "error"

    describe "validate" $ do
      it "returns Success when predicate passes" $ do
        validate "must be positive" (> 0) (5 :: Int) `shouldBe` Success 5

      it "returns Failure when predicate fails" $ do
        validate "must be positive" (> 0) (-5 :: Int) `shouldBe` Failure "must be positive"

    describe "conversions" $ do
      it "converts Success to Right" $ do
        validationToEither (Success 42 :: Validation String Int) `shouldBe` Right 42

      it "converts Failure to Left" $ do
        validationToEither (Failure "error" :: Validation String Int) `shouldBe` Left "error"

      it "converts Right to Success" $ do
        eitherToValidation (Right 42 :: Either String Int) `shouldBe` Success 42

      it "converts Left to Failure" $ do
        eitherToValidation (Left "error" :: Either String Int) `shouldBe` Failure "error"

      it "roundtrips Validation -> Either -> Validation" $ property $
        \(x :: Either String Int) ->
          validationToEither (eitherToValidation x) == x

    describe "BiFunctor instance" $ do
      it "maps over Failure with first function" $ do
        bimap length (+1) (Failure "error" :: Validation String Int) `shouldBe` Failure 5

      it "maps over Success with second function" $ do
        bimap length (+1) (Success 41 :: Validation String Int) `shouldBe` Success 42

  describe "Derived instances" $ do
    describe "Tri" $ do
      it "has Show instance" $ do
        show (TriA 1 :: Tri Int Int Int) `shouldBe` "TriA 1"
        show (TriB 2 :: Tri Int Int Int) `shouldBe` "TriB 2"
        show (TriC 3 :: Tri Int Int Int) `shouldBe` "TriC 3"

      it "has Eq instance" $ do
        (TriA 1 :: Tri Int Int Int) `shouldBe` TriA 1
        (TriA 1 :: Tri Int Int Int) `shouldNotBe` TriA 2
        (TriA 1 :: Tri Int Int Int) `shouldNotBe` TriB 1

      it "has Ord instance" $ do
        (TriA 1 :: Tri Int Int Int) < TriB 1 `shouldBe` True
        (TriB 1 :: Tri Int Int Int) < TriC 1 `shouldBe` True

    describe "Quad" $ do
      it "has Show instance" $ do
        show (QuadA 1 :: Quad Int Int Int Int) `shouldBe` "QuadA 1"

      it "has Eq instance" $ do
        (QuadA 1 :: Quad Int Int Int Int) `shouldBe` QuadA 1
        (QuadA 1 :: Quad Int Int Int Int) `shouldNotBe` QuadB 1

    describe "Penta" $ do
      it "has Show instance" $ do
        show (PentaA 1 :: Penta Int Int Int Int Int) `shouldBe` "PentaA 1"
        show (PentaE 5 :: Penta Int Int Int Int Int) `shouldBe` "PentaE 5"

      it "has Eq instance" $ do
        (PentaA 1 :: Penta Int Int Int Int Int) `shouldBe` PentaA 1
        (PentaA 1 :: Penta Int Int Int Int Int) `shouldNotBe` PentaE 1

    describe "Validation" $ do
      it "has Show instance" $ do
        show (Success 42 :: Validation String Int) `shouldBe` "Success 42"
        show (Failure "err" :: Validation String Int) `shouldBe` "Failure \"err\""
