{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Unit tests for the deprecated "Toml.Type.PrefixTree" module.
module Test.Toml.Type.PrefixTree
    ( prefixTreeSpec
    ) where

import Data.List (sort)
import Test.Hspec (Spec, describe, it, shouldBe)

import Toml.Type.Key (pattern (:||))

import qualified Toml.Type.PrefixTree as Prefix


prefixTreeSpec :: Spec
prefixTreeSpec = describe "PrefixTree unit tests" $ do
    -- some test keys
    let a  = "a" :|| []
    let b  = "b" :|| []
    let c  = "c" :|| []
    let ab = "a" :|| ["b"]

    it "Lookup on empty map returns Nothing" $
        Prefix.lookup @Bool a mempty `shouldBe` Nothing
    it "Lookup in single map returns this element" $ do
        let t = Prefix.single a True
        Prefix.lookup a t `shouldBe` Just True
        Prefix.lookup b t `shouldBe` Nothing
    it "Lookup after insert returns this element" $ do
        let t = Prefix.insert a True mempty
        Prefix.lookup a t `shouldBe` Just True
        Prefix.lookup b t `shouldBe` Nothing
    it "Lookup after multiple non-overlapping inserts" $ do
        let t = Prefix.insert a True $ Prefix.insert b False mempty
        Prefix.lookup a t `shouldBe` Just True
        Prefix.lookup b t `shouldBe` Just False
        Prefix.lookup c t `shouldBe` Nothing
    it "Prefix lookup" $ do
        let t = Prefix.insert ab True mempty
        Prefix.lookup a  t `shouldBe` Nothing
        Prefix.lookup ab t `shouldBe` Just True
    it "Composite key lookup" $ do
        let t = Prefix.insert a True $ Prefix.insert ab False mempty
        Prefix.lookup a  t `shouldBe` Just True
        Prefix.lookup ab t `shouldBe` Just False
    it "toList lists every entry with its full key" $ do
        let entries = [(a, 1 :: Int), (ab, 2), (b, 3), ("b" :|| ["c", "d"], 4)]
        sort (Prefix.toList (Prefix.fromList entries)) `shouldBe` sort entries
