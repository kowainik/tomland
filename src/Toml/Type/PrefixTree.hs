{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE PatternSynonyms #-}

{- |
Copyright: (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Implementation of prefix tree for TOML AST.
-}

module Toml.Type.PrefixTree
    (
      -- * Non-empty prefix tree
      PrefixTree (..)
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
import Data.Foldable (foldl')
import Data.HashMap.Strict (HashMap)
import GHC.Generics (Generic)

import Toml.Type.Key (pattern (:||), Key, KeysDiff (..), Piece, Prefix, keysDiff, (<|))

import qualified Data.HashMap.Strict as HashMap


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
