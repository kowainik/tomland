module Test.Toml.Property
       ( assocLaw
       , identityLaw
       ) where

import Data.Semigroup (Semigroup ((<>)))
import Hedgehog (Gen, forAll, (===))

import Test.Toml.Gen (PropertyTest, prop)

{- | The semigroup associativity axiom:

@
x <> (y <> z) ≡ (x <> y) <> z
@

-}
assocLaw :: (Eq a, Show a, Semigroup a) => Gen a -> PropertyTest
assocLaw gen = prop "Semigroup associativity law" $ do
    x <- forAll gen
    y <- forAll gen
    z <- forAll gen

    (x <> (y <> z)) === ((x <> y) <> z)

{- | Identity law for Monoid

@
mempty <> x = x
x <> mempty = x
@

-}
identityLaw :: (Eq a, Show a, Monoid a) => Gen a -> PropertyTest
identityLaw gen = prop "Monoid identity laws" $ do
    x <- forAll gen

    x <> mempty === x
    mempty <> x === x
