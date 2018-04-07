module Test.Toml.PrefixTree.Property
       ( propertyTests
       ) where

import Hedgehog (Property, forAll, property, (===))
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)

import Test.Toml.Gen (genKey, genPrefixMap, genVal)

import qualified Toml.PrefixTree as Prefix

propertyTests :: [TestTree]
propertyTests = [insertLookup, insertInsert]

insertLookup, insertInsert :: TestTree
insertLookup = testProperty "lookup k (insert k v m) == Just v"     prop_InsertLookup
insertInsert = testProperty "insert x a . insert x b == insert x a" prop_InsertInsert

----------------------------------------------------------------------------
-- InsertLookup
----------------------------------------------------------------------------

prop_InsertLookup :: Property
prop_InsertLookup =  property $ do
    t   <- forAll $ genPrefixMap genVal
    key <- forAll genKey
    val <- forAll genVal

    Prefix.lookup key (Prefix.insert key val t) === Just val

    -- DEBUG: ensures that trees of depth at least 5 are generated
    -- assert $ depth prefMap < 5

----------------------------------------------------------------------------
-- InsertInsert
----------------------------------------------------------------------------

prop_InsertInsert :: Property
prop_InsertInsert =  property $ do
    t <- forAll $ genPrefixMap genVal
    x <- forAll genKey
    a <- forAll genVal
    b <- forAll genVal

    Prefix.lookup x (Prefix.insert x a $ Prefix.insert x b t) === Just a

----------------------------------------------------------------------------
-- DEBUG
----------------------------------------------------------------------------

-- useful functions to test generators
-- TODO: commented to avoid warnings

-- depth :: PrefixMap a -> Int
-- depth = HashMap.foldl' (\acc t -> max acc (depthT t)) 0
--
-- depthT :: PrefixTree a -> Int
-- depthT (Leaf _ _)           = 1
-- depthT (Branch _ _ prefMap) = 1 + depth prefMap
