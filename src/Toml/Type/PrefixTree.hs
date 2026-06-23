{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE PatternSynonyms #-}

{- |
Module                  : Toml.Type.PrefixTree
Copyright               : (c) 2018-2022 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Implementation of prefix tree for TOML AST.

@since 0.0.0
-}

module Toml.Type.PrefixTree
    (
      -- * Non-empty prefix tree
      PrefixTree (..)
    , singleT
    , insertT
    , lookupT
    , toListT
    , addPrefixT
    , differenceWithT

      -- * Prefix map that stores roots of 'PrefixTree'
    , PrefixMap
    , single
    , insert
    , lookup
    , fromList
    , toList
    , differenceWith
    ) where

import Prelude hiding (lookup)

import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.HashMap.Strict (HashMap)
import GHC.Generics (Generic)

import Toml.Type.Key (pattern (:||), Key, KeysDiff (..), Piece, Prefix, keysDiff, (<|))

import qualified Data.HashMap.Strict as HashMap


{- | Map of layer names and corresponding 'PrefixTree's.

@since 0.0.0
-}
type PrefixMap a = HashMap Piece (PrefixTree a)

{- | Data structure to represent table tree for @toml@.

@since 0.0.0
-}
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

-- | @since 0.3
instance Semigroup (PrefixTree a) where
    a <> b = foldl' (\tree (k, v) -> insertT k v tree) a (toListT b)

{- | Push 'Prefix' inside the given 'PrefixTree'.

@since 1.3.2.0
-}
addPrefixT :: Prefix -> PrefixTree a -> PrefixTree a
addPrefixT pref = \case
    Leaf k a -> Leaf (pref <> k) a
    Branch k ma pma -> Branch (pref <> k) ma pma

{- | Convert branches to 'Leaf' or remove them at all.

@since 1.3.2.0
-}
compressTree :: PrefixTree a -> Maybe (PrefixTree a)
compressTree = \case
    l@(Leaf _ _) -> Just l
    b@(Branch p ma pma) -> case HashMap.toList pma of
        [] -> ma >>= \a -> Just (Leaf p a)
        [(_, child)] -> case ma of
            Just _ -> Just b
            Nothing -> compressTree $ addPrefixT p child
        _ : _ : _ -> Just b

{- | Creates a 'PrefixTree' of one key-value element.

@since 0.0.0
-}
singleT :: Key -> a -> PrefixTree a
singleT = Leaf
{-# INLINE singleT #-}

{- | Creates a 'PrefixMap' of one key-value element.

@since 0.0.0
-}
single :: Key -> a -> PrefixMap a
single k@(p :|| _) = HashMap.singleton p . singleT k

{- | Inserts key-value element into the given 'PrefixTree'.

@since 0.0.0
-}
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

{- | Inserts key-value element into the given 'PrefixMap'.

@since 0.0.0
-}
insert :: Key -> a -> PrefixMap a -> PrefixMap a
insert k@(p :|| _) v prefMap = case HashMap.lookup p prefMap of
    Just tree -> HashMap.insert p (insertT k v tree) prefMap
    Nothing   -> HashMap.insert p (singleT k v) prefMap

{- | Looks up the value at a key in the 'PrefixTree'.

@since 0.0.0
-}
lookupT :: Key -> PrefixTree a -> Maybe a
lookupT lk (Leaf k v) = if lk == k then Just v else Nothing
lookupT lk (Branch pref mv prefMap) =
    case keysDiff pref lk of
        Equal       -> mv
        NoPrefix    -> Nothing
        Diff {}     -> Nothing
        SndIsPref _ -> Nothing
        FstIsPref k -> lookup k prefMap

{- | Looks up the value at a key in the 'PrefixMap'.

@since 0.0.0
-}
lookup :: Key -> PrefixMap a -> Maybe a
lookup k@(p :|| _) prefMap = HashMap.lookup p prefMap >>= lookupT k

{- | Constructs 'PrefixMap' structure from the given list of 'Key' and value pairs.

@since 0.0.0
-}
fromList :: [(Key, a)] -> PrefixMap a
fromList = foldl' insertPair mempty
  where
    insertPair :: PrefixMap a -> (Key, a) -> PrefixMap a
    insertPair prefMap (k, v) = insert k v prefMap

{- | Converts 'PrefixTree' to the list of pairs.

@since 0.0.0
-}
toListT :: PrefixTree a -> [(Key, a)]
toListT (Leaf k v) = [(k, v)]
toListT (Branch pref ma prefMap) = case ma of
    Just a  -> (:) (pref, a)
    Nothing -> id
    $ map (first ((<>) pref)) $ toList prefMap

{- | Converts 'PrefixMap' to the list of pairs.

@since 0.0.0
-}
toList :: PrefixMap a -> [(Key, a)]
toList = concatMap (\(p, tr) -> first (p <|) <$> toListT tr) . HashMap.toList

{- | Difference of two 'PrefixMap's. Returns elements of the first 'PrefixMap'
that are not existing in the second one.

@since 1.3.2.0
-}
differenceWith :: (a -> b -> Maybe a) -> PrefixMap a -> PrefixMap b -> PrefixMap a
differenceWith f = HashMap.differenceWith (differenceWithT f)

{- | Difference of two 'PrefixTree's. Returns elements of the first 'PrefixTree'
that are not existing in the second one.

@since 1.3.2.0
-}
differenceWithT :: (a -> b -> Maybe a) -> PrefixTree a -> PrefixTree b -> Maybe (PrefixTree a)
differenceWithT f pt1 pt2 = case (pt1, pt2) of
    (Leaf k1 a, Leaf k2 b)
        | k1 == k2 -> f a b >>= \aNew -> Just (Leaf k1 aNew)
        | otherwise -> Just (Leaf k1 a)

    (l@(Leaf k a), Branch p mb pmb) -> case keysDiff k p of
        Equal -> mb >>= f a >>= \aNew -> Just (Leaf k aNew)
        NoPrefix -> Just l
        FstIsPref _ -> Just l
        SndIsPref kSuf -> case HashMap.toList $ differenceWith f (single kSuf a) pmb of
            -- zero elements
            [] -> Nothing
            -- our single key
            [(_, aNew)] -> Just $ addPrefixT k aNew
            -- shouldn't happen, but for some reasons
            _ : _ : _ -> Nothing
        Diff {} -> Just l

    (br@(Branch p ma pma), Leaf k b) -> case keysDiff p k of
        Equal -> compressTree $ Branch p (ma >>= \a -> f a b) pma
        NoPrefix -> Just br
        FstIsPref kSuf -> compressTree $ Branch p ma (differenceWith f pma $ single kSuf b)
        SndIsPref _ -> Just br
        Diff {} -> Just br

    (b1@(Branch p1 ma pma), Branch p2 mb pmb) -> case keysDiff p1 p2 of
        Equal -> compressTree $
            Branch p1 (ma >>= \a -> mb >>= \b -> f a b) (differenceWith f pma pmb)
        NoPrefix -> Just b1
        FstIsPref p2Suf@(p2Head :|| _) -> compressTree $
            Branch p1 ma (differenceWith f pma $ HashMap.singleton p2Head $ Branch p2Suf mb pmb)
        SndIsPref p1Suf@(p1Head :|| _) -> case HashMap.lookup p1Head pmb of
            Nothing -> Just b1
            Just ch -> addPrefixT p2 <$> differenceWithT f (Branch p1Suf ma pma) ch
        Diff {} -> Just b1
