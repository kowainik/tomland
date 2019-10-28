{- |
Copyright: (c) 2018-2019 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

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
import Data.Semigroup ((<>))
import Data.Text (Text)

import Toml.Parser.Core (Parser, alphaNumChar, char, lexeme, text)
import Toml.Parser.String (basicStringP, literalStringP)
import Toml.PrefixTree (Key (..), Piece (..))

import qualified Data.Text as Text


-- | Parser for bare key piece, like @foo@.
bareKeyPieceP :: Parser Text
bareKeyPieceP = lexeme $ Text.pack <$> bareStrP
  where
    bareStrP :: Parser String
    bareStrP = some $ alphaNumChar <|> char '_' <|> char '-'

-- | Parser for 'Piece'.
keyComponentP :: Parser Piece
keyComponentP = Piece <$>
    (bareKeyPieceP <|> (quote "\"" <$> basicStringP) <|> (quote "'" <$> literalStringP))
  where
    -- adds " or ' to both sides
    quote :: Text -> Text -> Text
    quote q t = q <> t <> q

-- | Parser for 'Key': dot-separated list of 'Piece'.
keyP :: Parser Key
keyP = Key <$> keyComponentP `sepBy1` char '.'

-- | Parser for table name: 'Key' inside @[]@.
tableNameP :: Parser Key
tableNameP = between (text "[") (text "]") keyP

-- | Parser for array of tables name: 'Key' inside @[[]]@.
tableArrayNameP :: Parser Key
tableArrayNameP = between (text "[[") (text "]]") keyP
