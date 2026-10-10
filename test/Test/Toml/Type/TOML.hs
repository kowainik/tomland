{-# OPTIONS_GHC -Wno-deprecations #-}

{- | Property tests for @TOML@ data type.
-}

module Test.Toml.Type.TOML
    ( tomlSpec
    ) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Test.Hspec (Spec, describe, it, parallel, shouldBe)

import Toml.Parser (parse)
import Toml.Type.AnyValue (AnyValue (..))
import Toml.Type.Edsl (mkToml, table, (=:))
import Toml.Type.TOML (TOML, tomlPairs, tomlTableArrays, tomlTables)
import Toml.Type.Value (Value (..))

import qualified Data.HashMap.Strict as HashMap
import qualified Toml.Type.PrefixTree as Prefix

import Test.Toml.Gen (genToml)
import Test.Toml.Property (assocSemigroup, leftIdentityMonoid, rightIdentityMonoid)


tomlSpec :: Spec
tomlSpec = parallel $ do
    describe "TOML laws" $ do
        assocSemigroup genToml
        rightIdentityMonoid genToml
        leftIdentityMonoid genToml
        it "Semigroup associativity with different kinds of entries at one key" $ do
            let a = mkToml $ table "k" ("p" =: 1)
                b = mkToml $ "k" =: 2
                c = mkToml $ table "k" ("q" =: 3)
            (a <> b) <> c `shouldBe` a <> (b <> c)
    accessorsSpec

-- | The deprecated accessors reproduce the three fields of tomland 1.3.
accessorsSpec :: Spec
accessorsSpec = describe "Deprecated accessors" $ do
    it "tomlPairs lists values with dotted keys flattened" $ do
        HashMap.lookup "x" (tomlPairs doc) `shouldBe` Just (AnyValue (Integer 1))
        HashMap.lookup "a.b" (tomlPairs doc) `shouldBe` Just (AnyValue (Integer 2))
        HashMap.size (tomlPairs doc) `shouldBe` 2
    it "tomlTables lists inline and header tables by their full key" $ do
        Prefix.lookup "t" (tomlTables doc) `shouldBe` Just (mkToml ("c" =: 3))
        Prefix.lookup "d.e" (tomlTables doc) `shouldBe` Just (mkToml ("f" =: 4))
        Prefix.lookup "d.h" (tomlTables doc) `shouldBe` Just mempty
        Prefix.lookup "d" (tomlTables doc) `shouldBe` Nothing
    it "tomlTableArrays lists arrays of tables" $
        HashMap.lookup "arr" (tomlTableArrays doc) `shouldBe` Just (mkToml ("g" =: 5) :| [])
    it "tomlTableArrays lists inline arrays of tables, tomlPairs does not" $ do
        let inlineDoc = either (error . show) id $ parse "x = [{ a = 1 }]"
        HashMap.lookup "x" (tomlTableArrays inlineDoc) `shouldBe` Just (mkToml ("a" =: 1) :| [])
        HashMap.lookup "x" (tomlPairs inlineDoc) `shouldBe` Nothing
  where
    doc :: TOML
    doc = either (error . show) id $ parse
        "x = 1\na.b = 2\nt = { c = 3 }\n[d.e]\nf = 4\n[[arr]]\ng = 5\n[d.h]\n"
