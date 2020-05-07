{- |
Copyright: (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Conversion from textual representation of @TOML@ values to the Haskell data
types by the given 'TomlCodec'.

This module includes coding functions like 'decode' and 'encode'.
-}

module Toml.Codec.Code
       ( -- * Decode
         decode
       , decodeFileEither
       , decodeFile
         -- * Encode
       , encode
       , encodeToFile
         -- * Run
       , runTomlCodec
       , execTomlCodec
       ) where

import Control.Exception (throwIO)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (runReader)
import Control.Monad.State (execState)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Bifunctor (first)
import Data.Text (Text)

import Toml.Codec.Error (LoadTomlException (..), TomlDecodeError (..), prettyTomlDecodeError)
import Toml.Codec.Types (Codec (..), TomlCodec)
import Toml.Parser (parse)
import Toml.Printer (pretty)
import Toml.Type (TOML (..))

import qualified Data.Text.IO as TIO


{- | Convert textual representation of @TOML@ into user data type by the
provided codec.
-}
decode :: TomlCodec a -> Text -> Either TomlDecodeError a
decode codec text = do
    toml <- first ParseError (parse text)
    runTomlCodec codec toml

{- | Similar to 'decode', but takes a path to a file with textual @TOML@
values from which it decodes them with the provided codec.

@since 1.3.0.0
-}
decodeFileEither
    :: forall a m . (MonadIO m)
    => TomlCodec a
    -> FilePath
    -> m (Either TomlDecodeError a)
decodeFileEither codec filePath = decode codec <$> liftIO (TIO.readFile filePath)

{- | Similar to 'decodeFileEither', throws 'LoadTomlException' in case of parse
errors ('TomlDecodeError').
-}
decodeFile :: forall a m . (MonadIO m) => TomlCodec a -> FilePath -> m a
decodeFile codec filePath = decodeFileEither codec filePath >>= errorWhenLeft
  where
    errorWhenLeft :: Either TomlDecodeError a -> m a
    errorWhenLeft (Left e)   = liftIO $ throwIO $ LoadTomlException filePath $
        prettyTomlDecodeError e
    errorWhenLeft (Right pc) = pure pc

-- | Convert data type to the textual representation of @TOML@ values.
encode :: TomlCodec a -> a -> Text
encode codec obj = pretty $ execTomlCodec codec obj

{- | Convert data type to the textual representation of @TOML@ values.
and write it info the given file.

@since 1.3.0.0
-}
encodeToFile :: forall a m . (MonadIO m) => TomlCodec a -> FilePath -> a -> m Text
encodeToFile codec filePath obj = content <$ liftIO (TIO.writeFile filePath content)
  where
    content :: Text
    content = encode codec obj

-- | Convert toml into user data type.
runTomlCodec :: TomlCodec a -> TOML -> Either TomlDecodeError a
runTomlCodec codec = runReader (runExceptT $ codecRead codec)

-- | Runs 'codecWrite' of 'TomlCodec' and returns intermediate TOML AST.
execTomlCodec :: TomlCodec a -> a -> TOML
execTomlCodec codec obj = execState (runMaybeT $ codecWrite codec obj) mempty
