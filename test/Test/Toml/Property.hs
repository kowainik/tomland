module Test.Toml.Property
       ( assocSemigroup
       , rightIdentityMonoid
       , leftIdentityMonoid
       ) where

import Data.Semigroup (Semigroup ((<>)))
import Hedgehog (Gen, forAll, (===))
import Test.Hspec (Arg, Expectation, SpecWith, it)
import Test.Hspec.Hedgehog (hedgehog)


{- | The semigroup associativity axiom:

@
x <> (y <> z) ≡ (x <> y) <> z
@
-}
assocSemigroup
    :: (Eq a, Show a, Semigroup a)
    => Gen a
    -> SpecWith (Arg Expectation)
assocSemigroup gen = it "Semigroup associativity: x <> (y <> z) ≡ (x <> y) <> z" $ hedgehog $ do
    x <- forAll gen
    y <- forAll gen
    z <- forAll gen

    (x <> (y <> z)) === ((x <> y) <> z)

{- | Right Identity law for Monoid

@
x <> mempty ≡ x
@
-}
rightIdentityMonoid
    :: (Eq a, Show a, Semigroup a, Monoid a)
    => Gen a
    -> SpecWith (Arg Expectation)
rightIdentityMonoid gen = it "Monoid Right Identity: x <> mempty ≡ x" $ hedgehog $ do
    x <- forAll gen
    x <> mempty === x

{- | Left Identity law for Monoid

@
mempty <> x ≡ x
@
-}
leftIdentityMonoid
    :: (Eq a, Show a, Semigroup a, Monoid a)
    => Gen a
    -> SpecWith (Arg Expectation)
leftIdentityMonoid gen = it "Monoid Right Identity: mempty <> x ≡ x" $ hedgehog $ do
    x <- forAll gen
    mempty <> x === x
