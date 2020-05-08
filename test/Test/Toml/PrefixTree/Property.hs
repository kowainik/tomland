module Test.Toml.PrefixTree.Property
    ( prefixTreePropertySpec
    ) where

import Data.String (IsString (..))
import Hedgehog (forAll, tripping, (===))
import Test.Hspec (Arg, Expectation, Spec, SpecWith, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

import Test.Toml.Gen (genKey, genPrefixMap, genVal)
import Test.Toml.Property (assocSemigroup, leftIdentityMonoid, rightIdentityMonoid)

import qualified Data.Text as Text
import qualified Toml.Type.PrefixTree as Prefix
import qualified Toml.Type.Printer as Printer


prefixTreePropertySpec :: Spec
prefixTreePropertySpec = describe "Prefix Tree property tests" $ do
    keyPrintSpec
    insertLookupSpec
    insertInsertSpec
    assocSemigroup      genPrefixMap
    leftIdentityMonoid  genPrefixMap
    rightIdentityMonoid genPrefixMap

keyPrintSpec :: SpecWith (Arg Expectation)
keyPrintSpec = it "Key printing: fromString . prettyKey ≡ id" $ hedgehog $ do
    key <- forAll genKey
    tripping key Printer.prettyKey (Just . fromString . Text.unpack)

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
