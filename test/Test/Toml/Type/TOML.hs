{- | Property tests for @TOML@ data type.
-}

module Test.Toml.Type.TOML
    ( tomlSpec
    ) where

import Test.Hspec (Spec, describe)

import Test.Toml.Gen (genToml)
import Test.Toml.Property (assocSemigroup, leftIdentityMonoid, rightIdentityMonoid)


tomlSpec :: Spec
tomlSpec = describe "TOML laws" $ do
    assocSemigroup genToml
    rightIdentityMonoid genToml
    leftIdentityMonoid genToml
