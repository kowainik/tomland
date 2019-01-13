module Test.Toml.PrefixTree.Property where

import Hedgehog (forAll, tripping, (===))

import Test.Toml.Gen (PropertyTest, genKey, genPrefixMap, genVal, prop)
import Test.Toml.Property (assocLaw, identityLaw)

import Data.String (IsString (..))

import qualified Data.Text as Text
import qualified Toml.PrefixTree as Prefix
import qualified Toml.Printer as Printer

----------------------------------------------------------------------------
-- Key printing and parsing
----------------------------------------------------------------------------

test_KeyPrinting :: PropertyTest
test_KeyPrinting = prop "Key printing: fromString . prettyKey == id" $ do
    key <- forAll genKey
    tripping key Printer.prettyKey (Just . fromString . Text.unpack)

----------------------------------------------------------------------------
-- InsertLookup
----------------------------------------------------------------------------

test_PrefixTreeInsertLookup :: PropertyTest
test_PrefixTreeInsertLookup =  prop "lookup k (insert k v m) == Just v" $ do
    t   <- forAll genPrefixMap
    key <- forAll genKey
    val <- forAll genVal

    Prefix.lookup key (Prefix.insert key val t) === Just val

    -- DEBUG: ensures that trees of depth at least 5 are generated
    -- assert $ depth prefMap < 5

----------------------------------------------------------------------------
-- InsertInsert
----------------------------------------------------------------------------

test_PrefixTreeInsertInsert :: PropertyTest
test_PrefixTreeInsertInsert =  prop "insert x a . insert x b == insert x a" $ do
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

----------------------------------------------------------------------------
-- Laws
----------------------------------------------------------------------------

test_PrefixTreeAssocLaw :: PropertyTest
test_PrefixTreeAssocLaw = assocLaw genPrefixMap

test_PrefixTreeIdentityLaw :: PropertyTest
test_PrefixTreeIdentityLaw = identityLaw genPrefixMap
