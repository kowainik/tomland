{- |
Module                  : Toml.Parser.Key
Copyright               : (c) 2018-2022 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Parsers for keys and table names.

@since 1.2.0.0
-}

module Toml.Parser.Key
       ( keyP
       , tableNameP
       , tableArrayNameP
       ) where

import Control.Applicative (Alternative (..))
import Control.Applicative.Combinators.NonEmpty (sepBy1)
import Control.Monad.Combinators (between)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Text (Text)

import Toml.Parser.Core (Parser, lexeme, takeWhile1P, text)
import Toml.Parser.String (basicStringP, literalStringP)
import Toml.Type.Key (Key (..), Piece (..))


{- | Parser for bare key piece, like @foo@. Bare keys may only contain ASCII
letters, ASCII digits, underscores, and dashes.
-}
bareKeyPieceP :: Parser Text
bareKeyPieceP = lexeme $ takeWhile1P (Just "bare key character") isBareKeyChar
  where
    isBareKeyChar :: Char -> Bool
    isBareKeyChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '_' || c == '-'

-- | Parser for 'Piece'. Quoted pieces are stored without their quotes.
keyComponentP :: Parser Piece
keyComponentP = Piece <$> (bareKeyPieceP <|> basicStringP <|> literalStringP)

{- | Parser for 'Key': dot-separated list of 'Piece'. Whitespace around dots is
ignored.
-}
keyP :: Parser Key
keyP = Key <$> keyComponentP `sepBy1` text "."

-- | Parser for table name: 'Key' inside @[]@.
tableNameP :: Parser Key
tableNameP = between (text "[") (text "]") keyP

-- | Parser for array of tables name: 'Key' inside @[[]]@.
tableArrayNameP :: Parser Key
tableArrayNameP = between (text "[[") (text "]]") keyP
