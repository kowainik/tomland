{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE PatternSynonyms #-}

{- |
Copyright: (c) 2018-2019 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Implementation of prefix tree for TOML AST.
-}

module Toml.PrefixTree
       ( -- * Core types
         Piece (..)
       , Key (..)
       , Prefix
       , pattern (:||)
       , keyToText

         -- * Key difference
       , KeysDiff (..)
       , keysDiff

         -- * Non-empty prefix tree
       , PrefixTree (..)
       , (<|)
       , singleT
       , insertT
       , lookupT
       , toListT

         -- * Prefix map that stores roots of 'PrefixTree'
       , PrefixMap
       , single
       , insert
       , lookup
       , fromList
       , toList
       ) where

import Prelude hiding (lookup)

import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Data.Foldable (foldl')
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (Semigroup (..))
import Data.String (IsString (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text


-- | Represents the key piece of some layer.
newtype Piece = Piece
    { unPiece :: Text
    } deriving stock (Generic)
      deriving newtype (Show, Eq, Ord, Hashable, IsString, NFData)

{- | Key of value in @key = val@ pair. Represents as non-empty list of key
components — 'Piece's. Key like

@
site."google.com"
@

is represented like

@
Key (Piece "site" :| [Piece "\\"google.com\\""])
@
-}
newtype Key = Key
    { unKey :: NonEmpty Piece
    } deriving stock (Generic)
      deriving newtype (Show, Eq, Ord, Hashable, NFData, Semigroup)

{- | Split a dot-separated string into 'Key'. Empty string turns into a 'Key'
with single element — empty 'Piece'.

This instance is not safe for now. Use carefully. If you try to use as a key
string like this @site.\"google.com\"@ you will have list of three components
instead of desired two.
-}
instance IsString Key where
    fromString :: String -> Key
    fromString = \case
        "" -> Key ("" :| [])
        s  -> case Text.splitOn "." (fromString s) of
            []   -> error "Text.splitOn returned empty string"  -- can't happen
            x:xs -> coerce @(NonEmpty Text) @Key (x :| xs)

{- | Bidirectional pattern synonym for constructing and deconstructing 'Key's.
-}
pattern (:||) :: Piece -> [Piece] -> Key
pattern x :|| xs <- Key (x :| xs)
  where
    x :|| xs = Key (x :| xs)

{-# COMPLETE (:||) #-}

keyToText :: Key -> Text
keyToText = Text.intercalate "." . NonEmpty.toList . coerce

-- | Type synonym for 'Key'.
type Prefix = Key

-- | Map of layer names and corresponding 'PrefixTree's.
type PrefixMap a = HashMap Piece (PrefixTree a)

-- | Data structure to represent table tree for @toml@.
data PrefixTree a
    = Leaf             -- ^ End of a key.
        !Key           -- ^ End piece of the key.
        !a             -- ^ Value at the end.
    | Branch           -- ^ Values along pieces of a key.
        !Prefix        -- ^ Greatest common key prefix.
        !(Maybe a)     -- ^ Possible value at that point.
        !(PrefixMap a) -- ^ Values at suffixes of the prefix.
    deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

instance Semigroup (PrefixTree a) where
    a <> b = foldl' (\tree (k, v) -> insertT k v tree) a (toListT b)

-- | Data structure to compare keys.
data KeysDiff
    = Equal      -- ^ Keys are equal
    | NoPrefix   -- ^ Keys don't have any common part.
    | FstIsPref  -- ^ The first key is the prefix of the second one.
        !Key     -- ^ Rest of the second key.
    | SndIsPref  -- ^ The second key is the prefix of the first one.
        !Key     -- ^ Rest of the first key.
    | Diff       -- ^ Key have a common prefix.
        !Key     -- ^ Common prefix.
        !Key     -- ^ Rest of the first key.
        !Key     -- ^ Rest of the second key.
    deriving stock (Show, Eq)

-- | Compares two keys
keysDiff :: Key -> Key -> KeysDiff
keysDiff (x :|| xs) (y :|| ys)
    | x == y    = listSame xs ys []
    | otherwise = NoPrefix
  where
    listSame :: [Piece] -> [Piece] -> [Piece] -> KeysDiff
    listSame [] []     _ = Equal
    listSame [] (s:ss) _ = FstIsPref $ s :|| ss
    listSame (f:fs) [] _ = SndIsPref $ f :|| fs
    listSame (f:fs) (s:ss) pr =
        if f == s
        then listSame fs ss (pr ++ [f])
        else Diff (x :|| pr) (f :|| fs) (s :|| ss)

-- | Prepends 'Piece' to the beginning of the 'Key'.
(<|) :: Piece -> Key -> Key
(<|) p k = Key (p NonEmpty.<| unKey k)
{-# INLINE (<|) #-}

-- | Creates a 'PrefixTree' of one key-value element.
singleT :: Key -> a -> PrefixTree a
singleT = Leaf
{-# INLINE singleT #-}

-- | Creates a 'PrefixMap' of one key-value element.
single :: Key -> a -> PrefixMap a
single k@(p :|| _) = HashMap.singleton p . singleT k

-- | Inserts key-value element into the given 'PrefixTree'.
insertT :: Key -> a -> PrefixTree a -> PrefixTree a
insertT newK newV (Leaf k v) =
    case keysDiff k newK of
        Equal -> Leaf k newV
        NoPrefix -> error "Algorithm error: can't be equal prefixes in insertT:Leaf case"
        FstIsPref rK -> Branch k (Just v) $ single rK newV
        SndIsPref lK -> Branch newK (Just newV) $ single lK v
        Diff p k1@(f :|| _) k2@(s :|| _) ->
          Branch p Nothing $ HashMap.fromList [(f, Leaf k1 v), (s, Leaf k2 newV)]
insertT newK newV (Branch pref mv prefMap) =
    case keysDiff pref newK of
        Equal    -> Branch pref (Just newV) prefMap
        NoPrefix -> error "Algorithm error: can't be equal prefixes in insertT:Branch case"
        FstIsPref rK -> Branch pref mv $ insert rK newV prefMap
        SndIsPref lK@(piece :|| _) ->
            Branch newK (Just newV) $ HashMap.singleton piece (Branch lK mv prefMap)
        Diff p k1@(f :|| _) k2@(s :|| _) ->
            Branch p Nothing $ HashMap.fromList [ (f, Branch k1 mv prefMap)
                                                , (s, Leaf k2 newV)
                                                ]

-- | Inserts key-value element into the given 'PrefixMap'.
insert :: Key -> a -> PrefixMap a -> PrefixMap a
insert k@(p :|| _) v prefMap = case HashMap.lookup p prefMap of
    Just tree -> HashMap.insert p (insertT k v tree) prefMap
    Nothing   -> HashMap.insert p (singleT k v) prefMap

-- | Looks up the value at a key in the 'PrefixTree'.
lookupT :: Key -> PrefixTree a -> Maybe a
lookupT lk (Leaf k v) = if lk == k then Just v else Nothing
lookupT lk (Branch pref mv prefMap) =
    case keysDiff pref lk of
        Equal       -> mv
        NoPrefix    -> Nothing
        Diff _ _ _  -> Nothing
        SndIsPref _ -> Nothing
        FstIsPref k -> lookup k prefMap

-- | Looks up the value at a key in the 'PrefixMap'.
lookup :: Key -> PrefixMap a -> Maybe a
lookup k@(p :|| _) prefMap = HashMap.lookup p prefMap >>= lookupT k

-- | Constructs 'PrefixMap' structure from the given list of 'Key' and value pairs.
fromList :: [(Key, a)] -> PrefixMap a
fromList = foldl' insertPair mempty
  where
    insertPair :: PrefixMap a -> (Key, a) -> PrefixMap a
    insertPair prefMap (k, v) = insert k v prefMap

-- | Converts 'PrefixTree' to the list of pairs.
toListT :: PrefixTree a -> [(Key, a)]
toListT (Leaf k v) = [(k, v)]
toListT (Branch pref ma prefMap) = case ma of
    Just a  -> (:) (pref, a)
    Nothing -> id
    $ map (\(k, v) -> (pref <> k, v)) $ toList prefMap

-- | Converts 'PrefixMap' to the list of pairs.
toList :: PrefixMap a -> [(Key, a)]
toList = concatMap (\(p, tr) -> first (p <|) <$> toListT tr) . HashMap.toList
