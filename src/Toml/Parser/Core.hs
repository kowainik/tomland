module Toml.Parser.Core
       ( module Text.Megaparsec
       , module Text.Megaparsec.Char
       , module Text.Megaparsec.Char.Lexer
       , Parser
       , lexeme
       , sc
       , text
       ) where

import Control.Applicative (Alternative (empty))

import Data.Text (Text)
import Data.Void (Void)

import Text.Megaparsec (Parsec, anySingle, errorBundlePretty, match, parse, satisfy, try, (<?>))
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, eol, hexDigitChar, space, space1,
                             string, tab)
import Text.Megaparsec.Char.Lexer (binary, float, hexadecimal, octal, signed, skipLineComment,
                                   symbol, decimal)
import qualified Text.Megaparsec.Char.Lexer as L (lexeme, space)


-- The parser
type Parser = Parsec Void Text


-- space consumer
sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    lineComment  = skipLineComment "#"
    blockComment = empty


-- wrapper for consuming spaces after every lexeme (not before it!)
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


-- parser for "fixed" string
text :: Text -> Parser Text
text = symbol sc
