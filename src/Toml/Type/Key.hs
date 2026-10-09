{-# LANGUAGE PatternSynonyms #-}

{- |
Module                  : Toml.Type.Key
Copyright               : (c) 2018-2022 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Implementation of key type. The type is used for key-value pairs and
table names.

@since 1.3.0.0
-}

module Toml.Type.Key
    ( -- * Core types
      Key (..)
    , Prefix
    , Piece (..)
    , pattern (:||)
    , (<|)

      -- * Key difference
    , KeysDiff (..)
    , keysDiff
    ) where

import Control.DeepSeq (NFData)
import Data.Char (chr, digitToInt, isHexDigit, isSpace)
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty (..))
import Data.String (IsString (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text


{- | Represents the key piece of some layer. The text is the key itself,
without quotes: the pieces of @site."google.com"@ are @site@ and
@google.com@. Whether a piece needs to be quoted when printed is decided by
the printer.

@since 1.4.0.0: quotes are no longer part of the piece.
@since 0.0.0
-}
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
Key (Piece "site" :| [Piece "google.com"])
@

@since 0.0.0
-}
newtype Key = Key
    { unKey :: NonEmpty Piece
    } deriving stock (Generic)
      deriving newtype (Show, Eq, Ord, Hashable, NFData, Semigroup)

{- | Type synonym for 'Key'.

@since 0.0.0
-}
type Prefix = Key

{- | Split a dot-separated string into 'Key'. Empty string turns into a 'Key'
with single element — empty 'Piece'. Quoted pieces are supported: the string
@site.\"google.com\"@ gives the two pieces @site@ and @google.com@, and the
usual escape sequences of basic strings are interpreted inside double quotes
(an invalid @\\x@, @\\u@ or @\\U@ escape is an error). Whitespace around dots
is ignored.

@since 1.4.0.0: quoted pieces are recognised.
@since 0.1.0
-}
instance IsString Key where
    fromString :: String -> Key
    fromString s = case splitPieces s of
        []     -> Key ("" :| [])
        p : ps -> Key (fmap (Piece . Text.pack) (p :| ps))

{- | Splits a string into key pieces on dots that are outside quotes, and
strips the quotes.
-}
splitPieces :: String -> [String]
splitPieces = go
  where
    go :: String -> [String]
    go str = case dropWhile isSpace str of
        '"' : rest   -> let (piece, after) = basic rest in piece : next after
        '\'' : rest -> let (piece, after) = break (== '\'') rest in piece : next (drop 1 after)
        other        -> let (piece, after) = break (== '.') other
                        in trimEnd piece : next after

    -- after a piece: optional whitespace, then either end or a dot
    next :: String -> [String]
    next str = case dropWhile isSpace str of
        '.' : rest -> go rest
        _          -> []

    basic :: String -> (String, String)
    basic = \case
        []            -> ([], [])
        '"' : rest    -> ([], rest)
        '\\' : c : rest ->
            let (x, rest') = unescape c rest
                (piece, after) = basic rest'
            in (x : piece, after)
        c : rest      -> let (piece, after) = basic rest in (c : piece, after)

    -- the character after a backslash, and the rest of the string
    unescape :: Char -> String -> (Char, String)
    unescape = \case
        'n' -> (,) '\n'
        't' -> (,) '\t'
        'r' -> (,) '\r'
        'b' -> (,) '\b'
        'f' -> (,) '\f'
        'e' -> (,) '\ESC'
        'x' -> hexEscape 'x' 2
        'u' -> hexEscape 'u' 4
        'U' -> hexEscape 'U' 8
        '"' -> (,) '"'
        '\\' -> (,) '\\'
        c   -> error $ "Invalid escape sequence in key: \\" <> [c]

    hexEscape :: Char -> Int -> String -> (Char, String)
    hexEscape prefix n str = case splitAt n str of
        (digits, rest)
            | length digits == n
            , all isHexDigit digits
            , Just c <- toUnicode (foldl (\acc d -> 16 * acc + digitToInt d) 0 digits)
            -> (c, rest)
            | otherwise
            -> error $ "Invalid escape sequence in key: \\" <> [prefix] <> digits

    -- Unicode scalar values, as in "Toml.Parser.String"
    toUnicode :: Int -> Maybe Char
    toUnicode x
        | x >= 0      && x <= 0xD7FF   = Just (chr x)
        | x >= 0xE000 && x <= 0x10FFFF = Just (chr x)
        | otherwise                    = Nothing

    trimEnd :: String -> String
    trimEnd = reverse . dropWhile isSpace . reverse

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

{- | Data represent difference between two keys.

@since 0.0.0
-}
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

{- | Find key difference between two keys.

@since 0.0.0
-}
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
