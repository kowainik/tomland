{- | Property tests for @TOML@ data type.
-}

module Test.Toml.Type.TOML
    ( tomlSpec
    ) where

import Test.Hspec (Spec, describe, parallel)

import Test.Toml.Gen (genToml)
import Test.Toml.Property (assocSemigroup, leftIdentityMonoid, rightIdentityMonoid)


tomlSpec :: Spec
tomlSpec = parallel $ describe "TOML laws" $ do
    assocSemigroup genToml
    rightIdentityMonoid genToml
    leftIdentityMonoid genToml
