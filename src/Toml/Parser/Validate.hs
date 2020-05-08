{- |
Copyright: (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

This module contains functions that aggregate the result of
'Toml.Parser.Item.tomlP' parser into 'TOML'. This approach allows to keep parser
fast and simple and delegate the process of creating tree structure to a
separate function.

@since 1.2.0.0
-}

module Toml.Parser.Validate
       ( -- * Decoding
         validateItems
       , ValidationError (..)

         -- * Internal helpers
       , groupItems
       , groupWithParent
       , validateItemForest
       ) where

import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Tree (Forest, Tree (..))

import Toml.Parser.Item (Table (..), TomlItem (..), setTableName)
import Toml.Type.Key (Key, KeysDiff (FstIsPref), keysDiff)
import Toml.Type.TOML (TOML (..), insertKeyAnyVal, insertTable, insertTableArrays)

import qualified Data.HashMap.Strict as HashMap
import qualified Toml.Type.PrefixTree as PrefixMap


{- | Validate list of 'TomlItem's and convert to 'TOML' if not validation
errors are found.
-}
validateItems :: [TomlItem] -> Either ValidationError TOML
validateItems = validateItemForest . groupItems

----------------------------------------------------------------------------
-- Grouping
----------------------------------------------------------------------------

{- | This function takes flat list of 'TomlItem's and groups it into list of
'Tree's by putting all corresponding items inside tables and table arrays.  It
doesn't perform any validation, just groups items according to prefixes of their
keys. So, for example, if you have the following keys as flat list:

@
aaa              # ordinary key
aaa.bbb          # ordinary key
[foo]            # table nam
foo.bar
foo.baz
[xxx]            # table name
[xxx.yyy]        # table name
zzz
@

the following tree structure will be created:

@
aaa
aaa.bbb
[foo]
├──── foo.bar
└──── foo.baz
[xxx]
└──── [yyy]
      └──── zzz
@
-}
groupItems :: [TomlItem] -> Forest TomlItem
groupItems = fst . groupWithParent Nothing

{- | This function groups list of TOML items into 'Forest' and returns list of
items that are not children of specified parent.

__Invariant:__ When this function is called with 'Nothing', second element in
the result tuple should be empty list.
-}
groupWithParent
    :: Maybe Key   -- ^ Parent name
    -> [TomlItem]  -- ^ List of items
    -> (Forest TomlItem, [TomlItem])  -- ^ Forest of times and remaining items
groupWithParent _ [] = ([], [])
groupWithParent parent (item:items) = case item of
    KeyVal{}            -> Node item [] <:> groupWithParent parent items
    InlineTable{}       -> Node item [] <:> groupWithParent parent items
    InlineTableArray{}  -> Node item [] <:> groupWithParent parent items
    TableName name      -> groupTable item name
    TableArrayName name -> groupTable item name
  where
    -- prepend to the first list, just to remove some code noise
    (<:>) :: a -> ([a], b) -> ([a], b)
    a <:> tup = first (a :) tup

    -- takes table item and its name, collects all children into table subforest
    -- and returns all elements after the table
    groupTable :: TomlItem -> Key -> (Forest TomlItem, [TomlItem])
    groupTable tableItem tableName = case parent of
        Nothing -> tableWithChildren tableName
        Just parentKey -> case keysDiff parentKey tableName of
            FstIsPref diff -> tableWithChildren diff
            _              -> ([], item:items)
      where
        tableWithChildren :: Key -> (Forest TomlItem, [TomlItem])
        tableWithChildren newName =
            let (children, rest) = groupWithParent (Just tableName) items
                newItem = setTableName newName tableItem
            in Node newItem children <:> groupWithParent parent rest

----------------------------------------------------------------------------
-- Decoding
----------------------------------------------------------------------------

{- | Error that happens during validating TOML which is already syntactically
correct. For the list of all possible validation errors and their explanation,
see the following issue on GitHub:

* https://github.com/kowainik/tomland/issues/5
-}

data ValidationError
    = DuplicateKey !Key
    | DuplicateTable !Key
    | SameNameKeyTable !Key
    | SameNameTableArray !Key
    deriving stock (Show, Eq)

{- | Construct 'TOML' from the 'Forest' of 'TomlItem' and performing validation
of TOML at the same time.
-}
validateItemForest :: Forest TomlItem -> Either ValidationError TOML
validateItemForest = go mempty
  where
    go :: TOML -> Forest TomlItem -> Either ValidationError TOML
    go toml [] = Right toml
    go toml@TOML{..} (node:nodes) = case rootLabel node of
        -- ignore subforest here
        KeyVal key val -> do
            HashMap.lookup key tomlPairs `errorOnJust` DuplicateKey key
            PrefixMap.lookup key tomlTables `errorOnJust` SameNameKeyTable key
            go (insertKeyAnyVal key val toml) nodes

        -- ignore subforest here
        InlineTable key table -> do
            HashMap.lookup key tomlPairs `errorOnJust` SameNameKeyTable key
            HashMap.lookup key tomlTableArrays `errorOnJust` SameNameTableArray key
            PrefixMap.lookup key tomlTables `errorOnJust` DuplicateTable key
            tableToml <- createTomlFromTable table
            go (insertTable key tableToml toml) nodes

        -- ignore subforest here
        InlineTableArray key tables -> do
            PrefixMap.lookup key tomlTables `errorOnJust` SameNameTableArray key
            arrayToml <- mapM createTomlFromTable tables
            go (insertTableArrays key arrayToml toml) nodes

        TableName key -> do
            HashMap.lookup key tomlPairs `errorOnJust` SameNameKeyTable key
            HashMap.lookup key tomlTableArrays `errorOnJust` SameNameTableArray key
            PrefixMap.lookup key tomlTables `errorOnJust` DuplicateTable key
            subTable <- go mempty (subForest node)
            go (insertTable key subTable toml) nodes

        TableArrayName key -> do
            PrefixMap.lookup key tomlTables `errorOnJust` SameNameTableArray key
            subTable <- go mempty (subForest node)
            let newArray = case HashMap.lookup key tomlTableArrays of
                    Nothing  -> HashMap.insert key (subTable :| []) tomlTableArrays
                    Just arr ->
                        HashMap.insert key (arr <> (subTable :| [])) tomlTableArrays
            go (toml { tomlTableArrays = newArray }) nodes

    createTomlFromTable :: Table -> Either ValidationError TOML
    createTomlFromTable (Table table) =
        go mempty $ map (\(k, v) -> Node (KeyVal k v) []) table



errorOnJust :: Maybe a -> e -> Either e ()
errorOnJust (Just _) e = Left e
errorOnJust Nothing  _ = Right ()
