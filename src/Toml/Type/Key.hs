{-# LANGUAGE PatternSynonyms #-}

{- |
Copyright: (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Implementation of key type. The type is used for key-value pairs and
table names.
-}

module Toml.Type.Key
    ( -- * Core types
      Key (..)
    , Prefix
    , Piece (..)
    , pattern (:||)
    , (<|)
    , keyToText

      -- * Key difference
    , KeysDiff (..)
    , keysDiff
    ) where

import Control.DeepSeq (NFData)
import Data.Coerce (coerce)
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty (..))
import Data.String (IsString (..))
import Data.Text (Text)
import GHC.Generics (Generic)

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

-- | Type synonym for 'Key'.
type Prefix = Key

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

-- | Prepends 'Piece' to the beginning of the 'Key'.
(<|) :: Piece -> Key -> Key
(<|) p k = Key (p NonEmpty.<| unKey k)
{-# INLINE (<|) #-}

-- | Convert 'Key' to human-readable 'Text'.
keyToText :: Key -> Text
keyToText = Text.intercalate "." . NonEmpty.toList . coerce
{-# INLINE keyToText #-}

-- | Data represent difference between two keys.
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

-- | Find key difference between two keys.
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
