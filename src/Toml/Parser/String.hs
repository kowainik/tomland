{- |
Copyright: (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Parsers for strings in TOML format, including basic and literal strings
both singleline and multiline.
-}

module Toml.Parser.String
       ( textP
       , basicStringP
       , literalStringP
       ) where

import Control.Applicative (Alternative (..))
import Control.Applicative.Combinators (count, manyTill, optional)
import Data.Char (chr, isControl)
import Data.Text (Text)

import Toml.Parser.Core (Parser, anySingle, char, eol, hexDigitChar, lexeme, satisfy, space, string,
                         tab, try, (<?>))

import qualified Data.Text as Text


{- | Parser for TOML text. Includes:

1. Basic single-line string.
2. Literal single-line string.
3. Basic multiline string.
4. Literal multiline string.
-}
textP :: Parser Text
textP = (multilineBasicStringP   <?> "multiline basic string")
    <|> (multilineLiteralStringP <?> "multiline literal string")
    <|> (literalStringP          <?> "literal string")
    <|> (basicStringP            <?> "basic string")
    <?> "text"

{- | Parse a non-control character (control character is a non-printing
character of the Latin-1 subset of Unicode).
-}
nonControlCharP :: Parser Text
nonControlCharP = Text.singleton <$> satisfy (not . isControl) <?> "non-control char"

-- | Parse escape sequences inside basic strings.
escapeSequenceP :: Parser Text
escapeSequenceP = char '\\' *> anySingle >>= \case
    'b'  -> pure "\b"
    't'  -> pure "\t"
    'n'  -> pure "\n"
    'f'  -> pure "\f"
    'r'  -> pure "\r"
    '"'  -> pure "\""
    '\\' -> pure "\\"
    'u'  -> hexUnicodeP 4
    'U'  -> hexUnicodeP 8
    c    -> fail $ "Invalid escape sequence: " <> "\\" <> [c]
  where
    hexUnicodeP :: Int -> Parser Text
    hexUnicodeP n = count n hexDigitChar >>= \x -> case toUnicode $ hexToInt x of
        Just c  -> pure (Text.singleton c)
        Nothing -> fail $ "Invalid unicode character: \\"
            <> (if n == 4 then "u" else "U")
            <> x
      where
        hexToInt :: String -> Int
        hexToInt xs = read $ "0x" ++ xs

        toUnicode :: Int -> Maybe Char
        toUnicode x
            -- Ranges from "The Unicode Standard".
            -- See definition D76 in Section 3.9, Unicode Encoding Forms.
            | x >= 0      && x <= 0xD7FF   = Just (chr x)
            | x >= 0xE000 && x <= 0x10FFFF = Just (chr x)
            | otherwise                    = Nothing

-- | Parser for basic string in double quotes.
basicStringP :: Parser Text
basicStringP = lexeme $ mconcat <$> (char '"' *> charP `manyTill` char '"')
  where
    charP :: Parser Text
    charP = escapeSequenceP <|> nonControlCharP

-- | Parser for literal string in single quotes.
literalStringP :: Parser Text
literalStringP = lexeme $ Text.pack <$> (char '\'' *> nonEolCharP `manyTill` char '\'')
  where
    nonEolCharP :: Parser Char
    nonEolCharP = satisfy (\c -> c /= '\n' && c /= '\r')

-- | Generic parser for multiline string. Used in 'multilineBasicStringP' and
-- 'multilineLiteralStringP'.
multilineP :: Parser Text -> Parser Text -> Parser Text
multilineP quotesP allowedCharP = lexeme $ mconcat <$>
    (quotesP *> optional eol *> allowedCharP `manyTill` quotesP)

-- Parser for basic multiline string in """ quotes.
multilineBasicStringP :: Parser Text
multilineBasicStringP = multilineP quotesP allowedCharP
  where
    quotesP :: Parser Text
    quotesP = string "\"\"\""

    allowedCharP :: Parser Text
    allowedCharP = lineEndingBackslashP <|> escapeSequenceP <|> nonControlCharP <|> eol

    lineEndingBackslashP :: Parser Text
    lineEndingBackslashP = Text.empty <$ try (char '\\' >> eol >> space)

-- Parser for literal multiline string in ''' quotes.
multilineLiteralStringP :: Parser Text
multilineLiteralStringP = multilineP quotesP allowedCharP
  where
    quotesP :: Parser Text
    quotesP = string "'''"

    allowedCharP :: Parser Text
    allowedCharP = nonControlCharP <|> eol <|> Text.singleton <$> tab
