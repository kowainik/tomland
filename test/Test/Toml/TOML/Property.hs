{- | Property tests on laws for @TOML@ data type.
-}
module Test.Toml.TOML.Property
    ( tomlLawsSpec
    ) where

import Test.Hspec (Spec, describe)

import Test.Toml.Gen (genToml)
import Test.Toml.Property (assocSemigroup, leftIdentityMonoid, rightIdentityMonoid)


tomlLawsSpec :: Spec
tomlLawsSpec = describe "TOML laws" $ do
    assocSemigroup genToml
    rightIdentityMonoid genToml
    leftIdentityMonoid genToml
