{- |
Copyright: (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

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
       , text
       ) where

import Control.Applicative (Alternative (empty))

import Data.Text (Text)
import Data.Void (Void)

import Text.Megaparsec (Parsec, anySingle, eof, errorBundlePretty, match, parse, satisfy, try,
                        (<?>))
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, eol, hexDigitChar, octDigitChar, binDigitChar, space, space1,
                             string, tab)
import Text.Megaparsec.Char.Lexer (binary, float, hexadecimal, octal, signed, skipLineComment,
                                   symbol)
import qualified Text.Megaparsec.Char.Lexer as L (lexeme, space)


-- | The parser
type Parser = Parsec Void Text

-- | Space and comment consumer. Currently also consumes newlines.
sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    lineComment  = skipLineComment "#"
    blockComment = empty

{- | Wrapper for consuming spaces after every lexeme (not before it!). Consumes
all characters according to 'sc' parser.
-}
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | 'Parser' for "fixed" string.
text :: Text -> Parser Text
text = symbol sc

hexStringToInt :: String -> Int
hexStringToInt [] = 0
hexStringToInt (x:xs) = 16 * (hexChar x) + hexStringToInt xs 

hexChar :: Char -> Int 
hexChar c 
   | c == '0' = 0
   | c == '1' = 1
   | c == '2' = 2
   | c == '3' = 3
   | c == '4' = 4
   | c == '5' = 5
   | c == '6' = 6
   | c == '7' = 7
   | c == '8' = 8
   | c == '9' = 9
   | c == 'a' = 10
   | c == 'b' = 11
   | c == 'c' = 12
   | c == 'd' = 13
   | c == 'e' = 14
   | c == 'f' = 15
   | c == 'A' = 10
   | c == 'B' = 11
   | c == 'C' = 12
   | c == 'D' = 13
   | c == 'E' = 14
   | c == 'F' = 15
