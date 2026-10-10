{-# LANGUAGE GADTs           #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

{- |
Module                  : Toml.Type.TOML
Copyright               : (c) 2018-2022 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Type of TOML AST and functions for creating and querying it.

@since 0.0.0
-}

module Toml.Type.TOML
       ( -- * TOML AST
         -- | The types are defined in "Toml.Type.Value", next to 'Value', because
         -- 'Value' and 'TOML' refer to each other.
         TOML (..)
       , Entry (..)
       , TableKind (..)

         -- * Queries
       , lookupEntry
       , lookupValue
       , lookupTable
       , lookupTableArray
       , entryKeys

         -- * Insertion
       , insertKeyVal
       , insertKeyAnyVal
       , insertTable
       , insertTableArrays
       , insertEntry
       , alterEntry

         -- * Difference
       , tomlDiff

         -- * Deprecated accessors
       , tomlPairs
       , tomlTables
       , tomlTableArrays
       ) where

import Data.Bifunctor (first)
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isNothing)

import Toml.Type.Key (Key (..), Piece, pattern (:||), (<|))
import Toml.Type.PrefixTree (PrefixMap)
import Toml.Type.Value (AnyValue (..), Entry (..), TOML (..), TableKind (..), Value)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.NonEmpty as NE
import qualified Toml.Type.PrefixTree as Prefix
import qualified Toml.Type.Value as Value



----------------------------------------------------------------------------
-- Queries
----------------------------------------------------------------------------

{- | Looks up the entry at the given (possibly dotted) key, walking through
sub-tables of any kind.

@since 1.4.0.0
-}
lookupEntry :: Key -> TOML -> Maybe Entry
lookupEntry (p :|| ps) (TOML entries) = HashMap.lookup p entries >>= \case
    entry | null ps -> Just entry
    ETable _ toml -> lookupEntry (Key $ NE.fromList ps) toml
    _ -> Nothing

{- | Looks up a value at the given key.

@since 1.4.0.0
-}
lookupValue :: Key -> TOML -> Maybe AnyValue
lookupValue key toml = lookupEntry key toml >>= \case
    EValue v -> Just v
    _        -> Nothing
{-# INLINE lookupValue #-}

{- | Looks up a table (of any kind) at the given key.

@since 1.4.0.0
-}
lookupTable :: Key -> TOML -> Maybe TOML
lookupTable key toml = lookupEntry key toml >>= \case
    ETable _ t -> Just t
    _          -> Nothing
{-# INLINE lookupTable #-}

{- | Looks up an array of tables at the given key. Both @[[key]]@ headers and
an inline array that contains only inline tables are returned.

@since 1.4.0.0
-}
lookupTableArray :: Key -> TOML -> Maybe (NonEmpty TOML)
lookupTableArray key toml = lookupEntry key toml >>= entryTableArray

{- | The tables of an entry that is an array of tables, written either with
@[[key]]@ headers or inline as @key = [ {..}, {..} ]@.
-}
entryTableArray :: Entry -> Maybe (NonEmpty TOML)
entryTableArray = \case
    ETableArray ts -> Just ts
    EValue (AnyValue (Value.Array elems)) -> NE.nonEmpty elems >>= traverse asTable
    _ -> Nothing
  where
    asTable :: AnyValue -> Maybe TOML
    asTable (AnyValue (Value.Table t)) = Just t
    asTable _                          = Nothing

{- | All keys of the table at the given key (or of the whole document for
an empty path), as single-piece 'Key's.

@since 1.4.0.0
-}
entryKeys :: TOML -> [Key]
entryKeys = map (:|| []) . HashMap.keys . unTOML
{-# INLINE entryKeys #-}

----------------------------------------------------------------------------
-- Insertion
----------------------------------------------------------------------------

{- | Modifies the entry at the given key. Missing intermediate tables are
created with the given 'TableKind'; existing intermediate entries that are
not tables are replaced by tables.

@since 1.4.0.0
-}
alterEntry :: TableKind -> (Maybe Entry -> Maybe Entry) -> Key -> TOML -> TOML
alterEntry kind f (p :|| ps) (TOML entries) = TOML $ case ps of
    []     -> HashMap.alter f p entries
    q : qs -> HashMap.alter (Just . step (Key (q :| qs))) p entries
  where
    step :: Key -> Maybe Entry -> Entry
    step rest = \case
        Just (ETable k toml) -> ETable k (alterEntry kind f rest toml)
        _                    -> ETable kind (alterEntry kind f rest mempty)

{- | Inserts an entry at the given key, replacing whatever was there. Missing
intermediate tables are created as 'DottedTable's for values and as
'ImplicitTable's for tables and arrays of tables.

@since 1.4.0.0
-}
insertEntry :: Key -> Entry -> TOML -> TOML
insertEntry key entry = alterEntry kind (const $ Just entry) key
  where
    kind :: TableKind
    kind = case entry of
        EValue _ -> DottedTable
        _        -> ImplicitTable

-- | Inserts given key-value into the 'TOML'.
insertKeyVal :: Key -> Value a -> TOML -> TOML
insertKeyVal k v = insertKeyAnyVal k (AnyValue v)
{-# INLINE insertKeyVal #-}

{- | Inserts given key-value into the 'TOML'. A 'Toml.Type.Value.Table' value
is stored as an 'InlineTable'.
-}
insertKeyAnyVal :: Key -> AnyValue -> TOML -> TOML
insertKeyAnyVal k = \case
    AnyValue (Value.Table t) -> insertEntry k (ETable InlineTable t)
    av                       -> insertEntry k (EValue av)
{-# INLINE insertKeyAnyVal #-}

-- | Inserts given table into the 'TOML' as a 'HeaderTable'.
insertTable :: Key -> TOML -> TOML -> TOML
insertTable k inToml = insertEntry k (ETable HeaderTable inToml)
{-# INLINE insertTable #-}

-- | Inserts given array of tables into the 'TOML'.
insertTableArrays :: Key -> NonEmpty TOML -> TOML -> TOML
insertTableArrays k arr = insertEntry k (ETableArray arr)
{-# INLINE insertTableArrays #-}

----------------------------------------------------------------------------
-- Difference
----------------------------------------------------------------------------

{- | Difference of two 'TOML's. Returns elements of the first 'TOML' that are
not existing in the second one.

@since 1.3.2.0
-}
tomlDiff :: TOML -> TOML -> TOML
tomlDiff (TOML t1) (TOML t2) = TOML $ HashMap.differenceWith entryDiff t1 t2
  where
    -- Nothing means: no difference, drop the entry
    entryDiff :: Entry -> Entry -> Maybe Entry
    entryDiff (ETable k a) (ETable _ b) = nonEmptyToml (ETable k) (tomlDiff a b)
    entryDiff a b
        -- @[[key]]@ headers and inline arrays of tables compare the same
        | Just as <- entryTableArray a, Just bs <- entryTableArray b =
            ETableArray <$> NE.nonEmpty (tomlListDiff (NE.toList as) (NE.toList bs))
        | a == b    = Nothing
        | otherwise = Just a

    nonEmptyToml :: (TOML -> Entry) -> TOML -> Maybe Entry
    nonEmptyToml mk toml
        | toml == mempty = Nothing
        | otherwise      = Just (mk toml)

    tomlListDiff :: [TOML] -> [TOML] -> [TOML]
    tomlListDiff [] _ = []
    tomlListDiff ts [] = ts
    tomlListDiff (a:as) (b:bs) = let diff = tomlDiff a b in
        if diff == mempty
        then tomlListDiff as bs
        else diff : tomlListDiff as bs

----------------------------------------------------------------------------
-- Deprecated accessors
----------------------------------------------------------------------------

{- | The key-value pairs of a document as a flat map, as the @tomlPairs@ field
of tomland 1.3 held them: values inside tables created by dotted keys (or
implicitly by headers) appear under their dotted key, so @a.b = 1@ yields the
key @a.b@. Values inside tables defined by headers or inline are not included,
nor are inline arrays of tables, which 'tomlTableArrays' lists.

@since 1.4.0.0
-}
tomlPairs :: TOML -> HashMap Key AnyValue
tomlPairs = HashMap.fromList . flatten
  where
    flatten :: TOML -> [(Key, AnyValue)]
    flatten = concatMap entry . HashMap.toList . unTOML

    entry :: (Piece, Entry) -> [(Key, AnyValue)]
    entry (p, e@(EValue v))
        | isNothing (entryTableArray e)            = [(p :|| [], v)]
    entry (p, ETable kind t) | isTransparent kind  = first (p <|) <$> flatten t
    entry _                                        = []
{-# DEPRECATED tomlPairs "Use lookupValue or lookupEntry; the flat map does not exist anymore since tomland-1.4." #-}

{- | The sub-tables of a document as a prefix map, as the @tomlTables@ field
of tomland 1.3 held them: tables defined by headers or inline, keyed by their
full key relative to this document.

@since 1.4.0.0
-}
tomlTables :: TOML -> PrefixMap TOML
tomlTables = Prefix.fromList . tables
  where
    tables :: TOML -> [(Key, TOML)]
    tables = concatMap entry . HashMap.toList . unTOML

    entry :: (Piece, Entry) -> [(Key, TOML)]
    entry (p, ETable kind t)
        | isTransparent kind = first (p <|) <$> tables t
        | otherwise          = [(p :|| [], t)]
    entry _ = []
{-# DEPRECATED tomlTables "Use lookupTable or lookupEntry; the prefix map does not exist anymore since tomland-1.4." #-}

{- | The arrays of tables of a document, as the @tomlTableArrays@ field of
tomland 1.3 held them, keyed by their full key relative to this document.
Both @[[key]]@ headers and inline arrays of tables are included.

@since 1.4.0.0
-}
tomlTableArrays :: TOML -> HashMap Key (NonEmpty TOML)
tomlTableArrays = HashMap.fromList . arrays
  where
    arrays :: TOML -> [(Key, NonEmpty TOML)]
    arrays = concatMap entry . HashMap.toList . unTOML

    entry :: (Piece, Entry) -> [(Key, NonEmpty TOML)]
    entry (p, e) | Just ts <- entryTableArray e    = [(p :|| [], ts)]
    entry (p, ETable kind t) | isTransparent kind  = first (p <|) <$> arrays t
    entry _                                        = []
{-# DEPRECATED tomlTableArrays "Use lookupTableArray or lookupEntry since tomland-1.4." #-}

-- | Tables that had no identity of their own in the tomland 1.3 representation.
isTransparent :: TableKind -> Bool
isTransparent kind = kind == DottedTable || kind == ImplicitTable
