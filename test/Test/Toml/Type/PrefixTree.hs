{-# LANGUAGE PatternSynonyms #-}

module Test.Toml.Type.PrefixTree
    ( prefixTreeSpec
    ) where

import Hedgehog (forAll, (===))
import Test.Hspec (Arg, Expectation, Spec, SpecWith, describe, it, shouldBe)
import Test.Hspec.Hedgehog (hedgehog)

import Test.Toml.Gen (genKey, genPrefixMap, genVal)
import Test.Toml.Property (assocSemigroup, leftIdentityMonoid, rightIdentityMonoid)
import Toml.Type.Key (pattern (:||))

import qualified Toml.Type.PrefixTree as Prefix


prefixTreeSpec :: Spec
prefixTreeSpec = describe "PrefixTree unit and property tests" $ do
    prefixTreeUnitSpec
    prefixTreePropertySpec

prefixTreeUnitSpec :: Spec
prefixTreeUnitSpec = describe "Unit tests for basic cases" $ do
    -- some test keys
    let a  = "a" :|| []
    let b  = "b" :|| []
    let c  = "c" :|| []
    let ab = "a" :|| ["b"]

    describe "Insert and lookup unit tests" $ do
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

prefixTreePropertySpec :: Spec
prefixTreePropertySpec = describe "Property tests on laws and expected behaviour" $ do
    insertLookupSpec
    insertInsertSpec
    assocSemigroup      genPrefixMap
    leftIdentityMonoid  genPrefixMap
    rightIdentityMonoid genPrefixMap

insertLookupSpec :: SpecWith (Arg Expectation)
insertLookupSpec = it "lookup k (insert k v m) ≡ Just v" $ hedgehog $ do
    t   <- forAll genPrefixMap
    key <- forAll genKey
    val <- forAll genVal

    Prefix.lookup key (Prefix.insert key val t) === Just val

    -- DEBUG: ensures that trees of depth at least 5 are generated
    -- assert $ depth prefMap < 5

insertInsertSpec :: SpecWith (Arg Expectation)
insertInsertSpec = it "insert x a . insert x b ≡ insert x a" $ hedgehog $ do
    t <- forAll genPrefixMap
    x <- forAll genKey
    a <- forAll genVal
    b <- forAll genVal

    Prefix.lookup x (Prefix.insert x a $ Prefix.insert x b t) === Just a

----------------------------------------------------------------------------
-- DEBUG
----------------------------------------------------------------------------

-- useful functions to test generators
-- uncomment when you need them

-- depth :: PrefixMap a -> Int
-- depth = HashMap.foldl' (\acc t -> max acc (depthT t)) 0
--
-- depthT :: PrefixTree a -> Int
-- depthT (Leaf _ _)           = 1
-- depthT (Branch _ _ prefMap) = 1 + depth prefMap
