{-# LANGUAGE PatternSynonyms #-}

{- |
Module                  : Toml.Parser.Validate
Copyright               : (c) 2018-2022 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

This module contains functions that aggregate the result of
'Toml.Parser.Item.tomlP' parser into 'TOML'. This approach allows to keep parser
fast and simple and delegate the process of creating tree structure to a
separate function.

The items are processed in document order, keeping track of the current
@[table]@ or @[[table]]@ header. Every table remembers how it was introduced
(see 'TableKind'), which is what the rules of the TOML specification about
redefining and extending tables are expressed in terms of.

@since 1.2.0.0
-}

module Toml.Parser.Validate
       ( -- * Decoding
         validateItems
       , ValidationError (..)

         -- * Internal helpers
       , validateValue
       ) where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty (..))

import Toml.Parser.Item (TomlItem (..))
import Toml.Type.Key (Key (..), Piece, pattern (:||))
import Toml.Type.TOML (Entry (..), TOML (..), TableKind (..))
import Toml.Type.UValue (UValue (..))
import Toml.Type.Value (AnyValue (..), Value (..))

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.NonEmpty as NE


{- | Error that happens during validating TOML which is already syntactically
correct. For the list of all possible validation errors and their explanation,
see the following issue on GitHub:

* https://github.com/kowainik/tomland/issues/5
-}
data ValidationError
    = DuplicateKey !Key
      -- ^ The same key is defined twice
    | DuplicateTable !Key
      -- ^ A table is defined twice: by two headers, or by a header after the
      -- table was already defined inline or by dotted keys
    | SameNameKeyTable !Key
      -- ^ A key and a table have the same name
    | SameNameTableArray !Key
      -- ^ A table and an array of tables have the same name
    | ExtendClosedTable !Key
      -- ^ A dotted key or header tries to add entries to a table that cannot
      -- be extended anymore: an inline table, or a table defined elsewhere
      --
      -- @since 1.4.0.0
    deriving stock (Show, Eq)

{- | Validate list of 'TomlItem's and convert to 'TOML' if not validation
errors are found.
-}
validateItems :: [TomlItem] -> Either ValidationError TOML
validateItems = go mempty Nothing
  where
    go :: TOML -> Maybe Key -> [TomlItem] -> Either ValidationError TOML
    go toml _ [] = Right toml
    go toml scope (item : items) = case item of
        TableName key ->
            defineTable key toml >>= \t -> go t (Just key) items
        TableArrayName key ->
            appendTableArray key toml >>= \t -> go t (Just key) items
        KeyVal key uval ->
            inScope scope (insertValue key uval) toml >>= \t -> go t scope items

{- | Converts an untyped 'UValue' into an 'AnyValue', validating the contents
of inline tables on the way.

@since 1.4.0.0
-}
validateValue :: UValue -> Either ValidationError AnyValue
validateValue = \case
    UBool b    -> pure $ AnyValue $ Bool b
    UInteger n -> pure $ AnyValue $ Integer n
    UDouble f  -> pure $ AnyValue $ Double f
    UText s    -> pure $ AnyValue $ Text s
    UZoned d   -> pure $ AnyValue $ Zoned d
    ULocal d   -> pure $ AnyValue $ Local d
    UDay d     -> pure $ AnyValue $ Day d
    UHours d   -> pure $ AnyValue $ Hours d
    UArray xs  -> AnyValue . Array <$> traverse validateValue xs
    UTable kvs -> AnyValue . Table <$> validateInline kvs

-- | Builds an inline table from its key-value pairs.
validateInline :: [(Key, UValue)] -> Either ValidationError TOML
validateInline = foldM (\toml (key, uval) -> insertValue key uval toml) mempty

----------------------------------------------------------------------------
-- Scopes
----------------------------------------------------------------------------

{- | Applies a modification inside the table of the current header (the last
element for an array of tables). Keys in errors are made absolute.
-}
inScope
    :: Maybe Key
    -> (TOML -> Either ValidationError TOML)
    -> TOML
    -> Either ValidationError TOML
inScope Nothing     f toml = f toml
inScope (Just path) f toml = first (prefixError path) (modifyAt path f toml)

-- | Applies a modification to the table at the given path, which must exist.
modifyAt :: forall e . Key -> (TOML -> Either e TOML) -> TOML -> Either e TOML
modifyAt (p :|| ps) f (TOML entries) = case HashMap.lookup p entries of
    Just (ETable kind t) -> wrap (ETable kind) <$> descend t
    Just (ETableArray ts) -> wrap (ETableArray . replaceLast ts) <$> descend (NE.last ts)
    -- the scope was created by a header, so this cannot happen; recover anyway
    _ -> wrap (ETable HeaderTable) <$> descend mempty
  where
    descend :: TOML -> Either e TOML
    descend t = case ps of
        []     -> f t
        q : qs -> modifyAt (Key (q :| qs)) f t

    wrap :: (TOML -> Entry) -> TOML -> TOML
    wrap mk t = TOML $ HashMap.insert p (mk t) entries

replaceLast :: NonEmpty a -> a -> NonEmpty a
replaceLast ts t = NE.fromList (NE.init ts ++ [t])

-- | Prepends the scope to the key mentioned in an error.
prefixError :: Key -> ValidationError -> ValidationError
prefixError path = \case
    DuplicateKey k       -> DuplicateKey (path <> k)
    DuplicateTable k     -> DuplicateTable (path <> k)
    SameNameKeyTable k   -> SameNameKeyTable (path <> k)
    SameNameTableArray k -> SameNameTableArray (path <> k)
    ExtendClosedTable k  -> ExtendClosedTable (path <> k)

----------------------------------------------------------------------------
-- Headers
----------------------------------------------------------------------------

{- | Defines a table for a @[a.b.c]@ header. Intermediate tables are created
as 'ImplicitTable's; an existing 'ImplicitTable' at the end of the path is
turned into a 'HeaderTable'.
-}
defineTable :: Key -> TOML -> Either ValidationError TOML
defineTable = walkHeader $ \here -> \case
    Nothing                       -> Right $ ETable HeaderTable mempty
    Just (ETable ImplicitTable t) -> Right $ ETable HeaderTable t
    Just (ETable _ _)             -> Left $ DuplicateTable here
    Just (ETableArray _)          -> Left $ SameNameTableArray here
    Just (EValue _)               -> Left $ SameNameKeyTable here

-- | Appends a new table to the array of tables for a @[[a.b.c]]@ header.
appendTableArray :: Key -> TOML -> Either ValidationError TOML
appendTableArray = walkHeader $ \here -> \case
    Nothing               -> Right $ ETableArray (mempty :| [])
    Just (ETableArray ts) -> Right $ ETableArray (ts <> (mempty :| []))
    Just (ETable _ _)     -> Left $ SameNameTableArray here
    Just (EValue _)       -> Left $ SameNameKeyTable here

{- | Walks the path of a header, descending into tables of any kind (except
inline tables) and into the last element of arrays of tables, and applies the
given function to the entry at the end of the path.
-}
walkHeader
    :: (Key -> Maybe Entry -> Either ValidationError Entry)
    -> Key
    -> TOML
    -> Either ValidationError TOML
walkHeader atEnd = go []
  where
    go :: [Piece] -> Key -> TOML -> Either ValidationError TOML
    go seen (p :|| ps) (TOML entries) = case ps of
        [] -> insertAt p entries <$> atEnd here (HashMap.lookup p entries)
        q : qs ->
            let continue = go (seen ++ [p]) (Key (q :| qs))
            in insertAt p entries <$> case HashMap.lookup p entries of
                Nothing                     -> ETable ImplicitTable <$> continue mempty
                Just (ETable InlineTable _) -> Left $ ExtendClosedTable here
                Just (ETable kind t)        -> ETable kind <$> continue t
                Just (ETableArray ts)       -> ETableArray . replaceLast ts <$> continue (NE.last ts)
                Just (EValue _)             -> Left $ SameNameKeyTable here
      where
        here :: Key
        here = absoluteKey seen p

----------------------------------------------------------------------------
-- Key/value pairs
----------------------------------------------------------------------------

{- | Inserts a @key = value@ pair into the current table. The pieces of a
dotted key create 'DottedTable's, and may only pass through 'DottedTable's
created in the same table.
-}
insertValue :: Key -> UValue -> TOML -> Either ValidationError TOML
insertValue key uval = go [] key
  where
    go :: [Piece] -> Key -> TOML -> Either ValidationError TOML
    go seen (p :|| ps) (TOML entries) = case ps of
        [] -> insertAt p entries <$> case HashMap.lookup p entries of
            Nothing -> first (prefixError here) (entryFromValue uval)
            Just (EValue _)
                | isTable uval -> Left $ SameNameKeyTable here
                | otherwise    -> Left $ DuplicateKey here
            Just (ETable _ _)
                | isTable uval -> Left $ DuplicateTable here
                | otherwise    -> Left $ SameNameKeyTable here
            Just (ETableArray _) -> Left $ SameNameTableArray here
        q : qs ->
            let continue = go (seen ++ [p]) (Key (q :| qs))
            in insertAt p entries <$> case HashMap.lookup p entries of
                Nothing                      -> ETable DottedTable <$> continue mempty
                Just (ETable DottedTable t)  -> ETable DottedTable <$> continue t
                Just (ETable _ _)            -> Left $ ExtendClosedTable here
                Just (ETableArray _)         -> Left $ SameNameTableArray here
                Just (EValue _)              -> Left $ SameNameKeyTable here
      where
        here :: Key
        here = absoluteKey seen p

    isTable :: UValue -> Bool
    isTable UTable{} = True
    isTable _        = False

-- | Converts a parsed value into a table entry.
entryFromValue :: UValue -> Either ValidationError Entry
entryFromValue = \case
    UTable kvs -> ETable InlineTable <$> validateInline kvs
    other      -> EValue <$> validateValue other

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

insertAt :: Piece -> HashMap Piece Entry -> Entry -> TOML
insertAt p entries entry = TOML $ HashMap.insert p entry entries

absoluteKey :: [Piece] -> Piece -> Key
absoluteKey seen p = Key $ NE.fromList (seen ++ [p])
