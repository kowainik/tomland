{- |
Module                  : Toml.Parser.String
Copyright               : (c) 2018-2022 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Parsers for strings in TOML format, including basic and literal strings
both singleline and multiline.
-}

module Toml.Parser.String
       ( textP
       , basicStringP
       , literalStringP
       ) where

import Control.Applicative (Alternative (..))
import Control.Applicative.Combinators (count, count', optional, skipMany)
import Control.Monad (void)
import Data.Char (chr)
import Data.Text (Text)

import Toml.Parser.Core (Parser, anySingle, char, eol, hexDigitChar, lexeme, satisfy, sc, string,
                         takeWhile1P, takeWhileP, try, (<?>))

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

{- | Whether a character may appear unescaped inside a string. Control
characters (U+0000 to U+001F and U+007F) are not allowed, except for tab. Any
other Unicode character (including non-ASCII ones) is allowed.
-}
isStringChar :: Char -> Bool
isStringChar c = c == '\t' || (c >= ' ' && c /= '\DEL')

-- | Parse escape sequences inside basic strings.
escapeSequenceP :: Parser Text
escapeSequenceP = char '\\' *> anySingle >>= \case
    'b'  -> pure "\b"
    't'  -> pure "\t"
    'n'  -> pure "\n"
    'f'  -> pure "\f"
    'r'  -> pure "\r"
    'e'  -> pure "\ESC"
    '"'  -> pure "\""
    '\\' -> pure "\\"
    'x'  -> hexUnicodeP 'x' 2
    'u'  -> hexUnicodeP 'u' 4
    'U'  -> hexUnicodeP 'U' 8
    c    -> fail $ "Invalid escape sequence: " <> "\\" <> [c]
  where
    hexUnicodeP :: Char -> Int -> Parser Text
    hexUnicodeP prefix n = count n hexDigitChar >>= \x -> case toUnicode $ hexToInt x of
        Just c  -> pure (Text.singleton c)
        Nothing -> fail $ "Invalid unicode character: \\" <> [prefix] <> x
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
basicStringP = lexeme $ mconcat <$> (char '"' *> many charP <* char '"')
  where
    charP :: Parser Text
    charP = escapeSequenceP <|> takeWhile1P (Just "basic string character") isBasicChar

    isBasicChar :: Char -> Bool
    isBasicChar c = isStringChar c && c /= '"' && c /= '\\'

-- | Parser for literal string in single quotes.
literalStringP :: Parser Text
literalStringP = lexeme $
    char '\'' *> takeWhileP (Just "literal string character") isLiteralChar <* char '\''
  where
    isLiteralChar :: Char -> Bool
    isLiteralChar c = isStringChar c && c /= '\''

{- | Generic parser for multiline string. Used in 'multilineBasicStringP' and
'multilineLiteralStringP'.

The closing delimiter is three quote characters; one or two additional quote
characters immediately before it are part of the string content, so that
e.g. @""""one quote""""@ is the string @"one quote"@.
-}
multilineP :: Char -> Parser Text -> Parser Text
multilineP quote allowedCharP = lexeme $ do
    _ <- string delimiter
    _ <- optional eol
    go []
  where
    delimiter :: Text
    delimiter = Text.replicate 3 (Text.singleton quote)

    -- closing delimiter followed by up to two extra quote characters
    closingP :: Parser Text
    closingP = string delimiter *> (Text.pack <$> count' 0 2 (char quote))

    go :: [Text] -> Parser Text
    go acc = (closingP >>= \extra -> pure (mconcat (reverse (extra : acc))))
        <|> (allowedCharP >>= \t -> go (t : acc))

-- Parser for basic multiline string in """ quotes.
multilineBasicStringP :: Parser Text
multilineBasicStringP = multilineP '"' allowedCharP
  where
    allowedCharP :: Parser Text
    allowedCharP = lineEndingBackslashP
        <|> escapeSequenceP
        <|> takeWhile1P (Just "basic string character") isBasicChar
        <|> Text.singleton <$> char '"'
        <|> eol

    isBasicChar :: Char -> Bool
    isBasicChar c = isStringChar c && c /= '"' && c /= '\\'

    -- A backslash which is the last non-whitespace character on a line
    -- trims all whitespace and newlines up to the next non-whitespace character.
    lineEndingBackslashP :: Parser Text
    lineEndingBackslashP = Text.empty <$ (try (char '\\' *> sc *> eol) *> skipWhitespaceAndNewlines)

    skipWhitespaceAndNewlines :: Parser ()
    skipWhitespaceAndNewlines = skipMany $
        void (satisfy (\c -> c == ' ' || c == '\t' || c == '\n')) <|> void (string "\r\n")

-- Parser for literal multiline string in ''' quotes.
multilineLiteralStringP :: Parser Text
multilineLiteralStringP = multilineP '\'' allowedCharP
  where
    allowedCharP :: Parser Text
    allowedCharP = takeWhile1P (Just "literal string character") isLiteralChar
        <|> Text.singleton <$> char '\''
        <|> eol

    isLiteralChar :: Char -> Bool
    isLiteralChar c = isStringChar c && c /= '\''
