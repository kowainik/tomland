module Toml.PrefixTree
       ( PrefixTree (..)
       , singleT
       , insertT
       , lookupT
       , deleteT

         -- * Types
       , Piece (..)
       , Key (..)
       , Prefix
       , KeysDiff (..)
       ) where

import Prelude hiding (lookup)

import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.NonEmpty as NonEmpty

-- | Represents the key piece of some layer.
newtype Piece = Piece { unPiece :: Text }
    deriving (Show, Eq, Hashable)

-- | Full representation of Key contains pieces of every layer.
newtype Key = Key { unKey :: NonEmpty Piece }
    deriving (Show, Eq)

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

toKey :: Piece -> [Piece] -> Key
toKey x xs = Key $ x :| xs

keysDiff :: Key -> Key -> KeysDiff
keysDiff (Key (x :| xs)) (Key (y :| ys))
    | x == y    = listSame xs ys []
    | otherwise = NoPrefix
  where
    listSame :: [Piece] -> [Piece] -> [Piece] -> KeysDiff
    listSame [] []     _ = Equal
    listSame [] (s:ss) _ = FstIsPref $ toKey s ss
    listSame (f:fs) [] _ = SndIsPref $ toKey f fs
    listSame (f:fs) (s:ss) pr =
        if f == s
        then listSame fs ss (pr ++ [f])
        else Diff (toKey x pr) (toKey f fs) (toKey s ss)

-- | Creates a 'PrefixTree' of one key-value element.
singleT :: Key -> a -> PrefixTree a
singleT = Leaf

-- | Inserts key-value element into the given 'PrefixTree'.
insertT :: Key -> a -> PrefixTree a -> PrefixTree a
insertT = undefined

-- | Looks up the value at a key in the 'PrefixTree'.
lookupT :: Key -> PrefixTree a -> Maybe a
lookupT lk (Leaf k v) = if lk == k then Just v else Nothing
lookupT lk (Branch pref mv prefMap) =
    case keysDiff pref lk of
        Equal               -> mv
        NoPrefix            -> Nothing
        Diff _ _ _          -> Nothing
        SndIsPref _         -> Nothing
        FstIsPref k@(Key r) -> HashMap.lookup (NonEmpty.head r) prefMap >>= lookupT k

-- | Remove a key and its value from the 'PrefixTree'. Returns the resulting 'PrefixTree'.
deleteT :: Key -> PrefixTree a -> PrefixTree a
deleteT = undefined
