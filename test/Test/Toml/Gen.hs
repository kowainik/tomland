{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | This module contains all generators for @tomland@ testing.

module Test.Toml.Gen
       ( genVal
       , genKey
       , genPrefixMap
       , genToml
       ) where

import Control.Monad (forM)

import Hedgehog (MonadGen)

import Toml.PrefixTree (pattern (:||), Key (..), Piece (..), PrefixMap, PrefixTree (..), fromList)
import Toml.Type (AnyValue (..), TOML (..), Value (..))

import qualified Data.HashMap.Strict as HashMap
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

----------------------------------------------------------------------------
-- Common generators
----------------------------------------------------------------------------

type V = Int

genVal :: MonadGen m => m V
genVal = Gen.int (Range.constant 0 256)

-- TODO: Arrays and Date.
-- | Generates random value of 'AnyValue' type.
genAnyValue :: MonadGen m => m AnyValue
genAnyValue = do
  randB <- Gen.bool
  randI <- toInteger <$> Gen.int (Range.constantBounded @Int)
  randF <- Gen.double $ Range.constant @Double (-1000000.0) 1000000.0
  randS <- Gen.text (Range.constant 0 256) Gen.alphaNum
  Gen.element
      [ AnyValue (Bool   randB)
      , AnyValue (Int    randI)
      , AnyValue (Float  randF)
      , AnyValue (String randS)
      ]

-- TODO: unicode support
genPiece :: MonadGen m => m Piece
genPiece = Piece <$> Gen.text (Range.constant 1 50) Gen.alphaNum

genKey :: MonadGen m => m Key
genKey = Key <$> Gen.nonEmpty (Range.constant 1 10) genPiece

genKeyAnyValue :: MonadGen m => m (Key, AnyValue)
genKeyAnyValue = do
    key <- genKey
    val <- genAnyValue
    pure (key, val)

genKeyAnyValueList :: MonadGen m => m [(Key, AnyValue)]
genKeyAnyValueList = Gen.list (Range.linear 0 10) genKeyAnyValue

-- Generates key-value pair for PrefixMap
genEntry :: MonadGen m => m (Piece, Key)
genEntry = do
    key@(piece :|| _) <- genKey
    pure (piece, key)

genPrefixMap :: MonadGen m => m v -> m (PrefixMap v)
genPrefixMap vGen = do
    entries <- Gen.list (Range.linear 0 10) genEntry
    kvps    <- forM entries $ \(piece, key) -> do
        tree <- genPrefixTree vGen key
        pure (piece, tree)

    pure $ HashMap.fromList kvps

genPrefixTree :: forall v m . MonadGen m => m v -> Key -> m (PrefixTree v)
genPrefixTree genV key = Gen.recursive
    -- list picker generator combinator
    Gen.choice
    -- non-recursive generators
    [ Leaf key <$> genV ]
    -- recursive generators
    [ genPrefixMap genV >>= genBranch ]
  where
    genBranch :: PrefixMap v -> m (PrefixTree v)
    genBranch prefMap = do
        prefVal <- Gen.maybe genV
        pure $ Branch key prefVal prefMap

genTableHeader :: MonadGen m => m (Key, TOML)
genTableHeader = do
    k <- genKey
    toml <- makeToml <$> genKeyAnyValueList
    pure (k, toml)
  where
    makeToml :: [(Key, AnyValue)] -> TOML
    makeToml kv = TOML (HashMap.fromList kv) mempty

genToml :: MonadGen m => m TOML
genToml = do
    kv     <- HashMap.fromList <$> genKeyAnyValueList
    tables <- Gen.list (Range.linear 0 10) genTableHeader
    pure $ TOML kv (fromList tables)
