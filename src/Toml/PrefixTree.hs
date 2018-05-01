{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Toml.PrefixTree
       ( PrefixTree (..)
       , singleT
       , insertT
       , lookupT

       , PrefixMap
       , single
       , insert
       , lookup
       , fromList

         -- * Types
       , Piece (..)
       , Key (..)
       , Prefix
       , KeysDiff (..)
       ) where

import Prelude hiding (lookup)

import Control.Arrow ((&&&))
import Data.Foldable (foldl')
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty (..))
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.NonEmpty as NonEmpty

-- | Represents the key piece of some layer.
newtype Piece = Piece { unPiece :: Text }
    deriving (Show, Eq, Ord, Hashable, IsString)

{- | Key of value in @key = val@ pair. Represents as non-empty list of key
components -- 'Piece's. Key like

@
site."google.com"
@

is represented like

@
Key (Piece "site" :| [Piece "\\"google.com\\""])
@

-}
newtype Key = Key { unKey :: NonEmpty Piece }
    deriving (Show, Eq, Ord, Generic)

instance Hashable Key

pattern (:||) :: Piece -> [Piece] -> Key
pattern x :|| xs <- ((NonEmpty.head &&& NonEmpty.tail) . unKey -> (x, xs))
  where
    x :|| xs = Key $ x :| xs

{-# COMPLETE (:||) #-}

-- | Type synonym for 'Key'.
type Prefix = Key

-- | Map of layer names and corresponding 'PrefixTree's.
type PrefixMap a = HashMap Piece (PrefixTree a)

-- | Data structure to represent table tree for @toml@.
data PrefixTree a
    = Leaf !Key !a
    | Branch { bCommonPref :: !Prefix         -- ^ greatest common prefix
             , bVal        :: !(Maybe a)      -- ^ value by key = prefix
             , bPrefixMap  :: !(PrefixMap a)  -- ^ suffixes of prefix
             }

data KeysDiff
      -- | Keys are equal
    = Equal
      -- | Keys don't have any common part.
    | NoPrefix
      -- | The first key is the prefix for the second one.
    | FstIsPref { diff :: !Key}
      -- | The second key is the prefix for the first one.
    | SndIsPref { diff :: !Key}
      -- | Key have same prefix.
    | Diff { pref    :: !Key
           , diffFst :: !Key
           , diffSnd :: !Key
           }
    deriving (Show, Eq)

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

-- | Creates a 'PrefixTree' of one key-value element.
singleT :: Key -> a -> PrefixTree a
singleT = Leaf

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

fromList :: [(Key, a)] -> PrefixMap a
fromList = foldl' insertPair mempty
  where
    insertPair :: PrefixMap a -> (Key, a) -> PrefixMap a
    insertPair prefMap (k, v) = insert k v prefMap
