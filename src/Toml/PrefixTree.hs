module Toml.PrefixTree
       ( PrefixTree (..)
       , single
       , insert
       , lookup
       , delete
       ) where

import Prelude hiding (lookup)

import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)

import qualified Data.HashMap.Strict as HashMap

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

-- | Data structure to represent @toml@.
data PrefixTree a
    = Leaf Key a
    | Branch { bcommonPref :: Prefix         -- ^ greatest common prefix
             , bVal        :: (Maybe a)      -- ^ value by key = prefix
             , bPrefixMap  :: (PrefixMap a)  -- ^ suffixes of prefix
             }

data KeysDiff
      -- | Keys are equal
    = Equal
      -- | Keys don't have any common part.
    | NoPrefix
      -- | The first key is the prefix for the second one.
    | FstIsPref { diff :: Key}
      -- | The second key is the prefix for the first one.
    | SndIsPref { diff :: Key}
      -- | Key have same prefix.
    | Diff { pref    :: Key
           , diffFst :: Key
           , diffSnd :: Key
           }
    deriving (Show, Eq)

toKey :: Piece -> [Piece] -> Key
toKey x xs = Key $ x :| xs

keysDiff :: Key -> Key -> KeysDiff
keysDiff (Key (x :| xs)) (Key (y :| ys)) =
    if x == y
    then listSame xs ys []
    else NoPrefix
  where
    listSame [] []     _ = Equal
    listSame [] (s:ss) _ = FstIsPref $ toKey s ss
    listSame (f:fs) [] _ = SndIsPref $ toKey f fs
    listSame (f:fs) (s:ss) pr =
        if f == s
        then listSame fs ss (pr ++ [f])
        else Diff (toKey x pr) (toKey f fs) (toKey s ss)

-- | Creates a 'PrefixTree' of one key-value element.
single :: Key -> a -> PrefixTree a
single = Leaf

-- | Inserts key-value element into the given 'PrefixTree'.
insert :: Key -> a -> PrefixTree a -> PrefixTree a
insert = undefined

-- | Looks up the value at a key in the 'PrefixTree'.
lookup :: Key -> PrefixTree a -> Maybe a
lookup lk (Leaf k v) = if lk == k then Just v else Nothing
lookup lk (Branch pref mv prefMap) =
    case keysDiff pref lk of
        Equal -> mv
        NoPrefix -> Nothing
        Diff _ _ _ -> Nothing
        SndIsPref _ -> Nothing
        FstIsPref (Key (r :| rs)) -> case HashMap.lookup r prefMap of
            Nothing -> Nothing
            Just pt -> case rs of
                           []     -> bVal pt
                           (x:xs) -> lookup (toKey x xs) pt


-- | Remove a key and its value from the 'PrefixTree'. Returns the deleted value if
-- operation succeeded or 'Nothing' in other case.
delete :: Key -> PrefixTree a -> Maybe (PrefixTree a)
delete = undefined
