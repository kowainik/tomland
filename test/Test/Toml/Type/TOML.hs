{- | Property tests for @TOML@ data type.
-}

module Test.Toml.Type.TOML
    ( tomlSpec
    ) where

import Test.Hspec (Spec, describe, it, parallel, shouldBe)

import Toml.Type.Edsl (mkToml, table, (=:))

import Test.Toml.Gen (genToml)
import Test.Toml.Property (assocSemigroup, leftIdentityMonoid, rightIdentityMonoid)


tomlSpec :: Spec
tomlSpec = parallel $ describe "TOML laws" $ do
    assocSemigroup genToml
    rightIdentityMonoid genToml
    leftIdentityMonoid genToml
    it "Semigroup associativity with different kinds of entries at one key" $ do
        let a = mkToml $ table "k" ("p" =: 1)
            b = mkToml $ "k" =: 2
            c = mkToml $ table "k" ("q" =: 3)
        (a <> b) <> c `shouldBe` a <> (b <> c)
