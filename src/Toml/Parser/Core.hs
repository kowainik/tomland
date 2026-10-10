{- |
Module                  : Toml.Parser.Core
Copyright               : (c) 2018-2022 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Core functions for TOML parser.
-}

module Toml.Parser.Core
       ( -- * Reexports from @megaparsec@
         module Text.Megaparsec
       , module Text.Megaparsec.Char
       , module Text.Megaparsec.Char.Lexer

         -- * Core parsers for TOML
       , Parser
       , lexeme
       , sc
       , scn
       , commentP
       , lineEndP
       , text
       ) where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (void)

import Data.Text (Text)
import Data.Void (Void)

import Text.Megaparsec (Parsec, anySingle, eof, errorBundlePretty, match, parse, satisfy,
                        takeWhile1P, takeWhileP, try, (<?>))
import Text.Megaparsec.Char (alphaNumChar, binDigitChar, char, digitChar, eol, hexDigitChar,
                             octDigitChar, space, space1, string, tab)
import Text.Megaparsec.Char.Lexer (binary, float, hexadecimal, octal, signed, skipLineComment,
                                   symbol)
import qualified Text.Megaparsec.Char.Lexer as L (lexeme, space)


-- | The parser
type Parser = Parsec Void Text

{- | Whitespace consumer. Consumes only /horizontal/ whitespace, i.e. spaces and
tabs. It does not consume newlines or comments, so parsers built on top of it
cannot accidentally span multiple lines: TOML requires a key, its @=@ and its
value to be on the same line, and table headers to be on a single line too.

Use 'scn' where newlines and comments are allowed, e.g. inside arrays and inline
tables and between top-level items.

@since 1.4.0.0
-}
sc :: Parser ()
sc = void $ takeWhileP (Just "white space") isHorizontalSpace

{- | Whitespace and comment consumer that also consumes newlines. Newlines are
LF or CRLF; a bare CR is not whitespace.

@since 1.4.0.0
-}
scn :: Parser ()
scn = L.space whitespace commentP empty
  where
    whitespace :: Parser ()
    whitespace = void (takeWhile1P (Just "white space") isSpaceOrLf) <|> void (string "\r\n")

    isSpaceOrLf :: Char -> Bool
    isSpaceOrLf c = isHorizontalSpace c || c == '\n'

{- | Parser for a comment: @#@ followed by any characters up to (but not
including) the end of the line. Control characters other than tab are not
allowed in comments.

@since 1.4.0.0
-}
commentP :: Parser ()
commentP = void $ char '#' *> takeWhileP (Just "comment character") isCommentChar
  where
    isCommentChar :: Char -> Bool
    isCommentChar c = c == '\t' || (c >= ' ' && c /= '\DEL')

{- | Parser for the end of a line: optional trailing comment followed by a
newline or the end of input. Any following blank lines and comments are
consumed as well.

@since 1.4.0.0
-}
lineEndP :: Parser ()
lineEndP = sc *> (commentP <|> pure ()) *> (void eol <|> eof) *> scn

isHorizontalSpace :: Char -> Bool
isHorizontalSpace c = c == ' ' || c == '\t'

{- | Wrapper for consuming spaces after every lexeme (not before it!). Consumes
all characters according to 'sc' parser.
-}
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | 'Parser' for "fixed" string.
text :: Text -> Parser Text
text = symbol sc
