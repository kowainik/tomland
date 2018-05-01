{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Toml.PrefixTree.Property
       ( propertyTests
       ) where

import Control.Monad (forM)

import Hedgehog (MonadGen, Property, forAll, property, (===))
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)

import Toml.PrefixTree (pattern (:||), Key (..), Piece (..), PrefixMap, PrefixTree (..))

import qualified Data.HashMap.Strict as HashMap
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Toml.PrefixTree as Prefix

propertyTests :: [TestTree]
propertyTests = [insertLookup, insertInsert]

insertLookup, insertInsert :: TestTree
insertLookup = testProperty "lookup k (insert k v m) == Just v"     prop_InsertLookup
insertInsert = testProperty "insert x a . insert x b == insert x a" prop_InsertInsert

----------------------------------------------------------------------------
-- Common generators
----------------------------------------------------------------------------

type V = Int

genVal :: MonadGen m => m V
genVal = Gen.int (Range.constant 0 256)

genPiece :: MonadGen m => m Piece
genPiece = Piece <$> Gen.text (Range.constant 1 50) Gen.unicode

genKey :: MonadGen m => m Key
genKey = Key <$> Gen.nonEmpty (Range.constant 1 10) genPiece

-- Generates key-value pair for PrefixMap
genEntry :: MonadGen m => m (Piece, Key)
genEntry = do
    key@(piece :|| _) <- genKey
    pure (piece, key)

genPrefixMap :: MonadGen m => m (PrefixMap V)
genPrefixMap = do
    entries <- Gen.list (Range.linear 0 10) genEntry
    kvps    <- forM entries $ \(piece, key) -> do
        tree <- genPrefixTree key
        pure (piece, tree)

    pure $ HashMap.fromList kvps

genPrefixTree :: forall m . MonadGen m => Key -> m (PrefixTree V)
genPrefixTree key = Gen.recursive
    -- list picker generator combinator
    Gen.choice
    -- non-recursive generators
    [ Leaf key <$> genVal ]
    -- recursive generators
    [ genPrefixMap >>= genBranch ]
  where
    genBranch :: PrefixMap V -> m (PrefixTree V)
    genBranch prefMap = do
        prefVal <- Gen.maybe genVal
        pure $ Branch key prefVal prefMap

----------------------------------------------------------------------------
-- InsertLookup
----------------------------------------------------------------------------

prop_InsertLookup :: Property
prop_InsertLookup =  property $ do
    t   <- forAll genPrefixMap
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
    t <- forAll genPrefixMap
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
