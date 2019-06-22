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
       , eof
       ) where

import Control.Applicative (Alternative (empty))

import Data.Text (Text)
import Data.Void (Void)

import Text.Megaparsec (Parsec, anySingle, eof, errorBundlePretty, match, parse, satisfy, try,
                        (<?>))
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, eol, hexDigitChar, space, space1,
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
