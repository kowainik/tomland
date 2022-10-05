{- |
Module                  : Toml.Codec.Code
Copyright               : (c) 2018-2022 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Conversion from textual representation of @TOML@ values to the Haskell data
types by the given 'TomlCodec'.

This module includes coding functions like 'decode' and 'encode'.
-}

module Toml.Codec.Code
       ( -- * Decode
         decode
       , decodeExact
       , decodeValidation
       , decodeFileEither
       , decodeFile
       , decodeFileExact
         -- * Encode
       , encode
       , encodeToFile
         -- * Run
       , runTomlCodec
       , execTomlCodec
       ) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import Validation (Validation (..), validationToEither)

import Toml.Codec.Error (LoadTomlException (..), TomlDecodeError (..), prettyTomlDecodeErrors)
import Toml.Codec.Types (Codec (..), TomlCodec, TomlState (..))
import Toml.Parser (parse)
import Toml.Type (TOML (..), tomlDiff)
import Toml.Type.Printer (pretty)

import qualified Data.Text.IO as TIO


{- | Convert textual representation of @TOML@ into user data type by the
provided codec.

@since 1.3.0.0
-}
decodeValidation :: TomlCodec a -> Text -> Validation [TomlDecodeError] a
decodeValidation codec text = case parse text of
    Left err   -> Failure [ParseError err]
    Right toml -> runTomlCodec codec toml

{- | Convert textual representation of @TOML@ into user data type by the
provided codec.

@since 0.0.0
-}
decode :: TomlCodec a -> Text -> Either [TomlDecodeError] a
decode codec = validationToEither . decodeValidation codec

{- | Convert textual representation of @TOML@ into user data type by the
provided codec.

Unlike 'decode', this function returns 'NotExactDecode' error in case if given
@TOML@ has redundant fields and other elements.

@since 1.3.2.0
-}
decodeExact :: TomlCodec a -> Text -> Either [TomlDecodeError] a
decodeExact codec text = case parse text of
    Left err -> Left [ParseError err]
    Right toml -> case runTomlCodec codec toml of
        Failure errs -> Left errs
        Success a ->
            let tomlExpected = execTomlCodec codec a
                aDiff = tomlDiff toml tomlExpected in
            if aDiff == mempty
            then Right a
            else Left [NotExactDecode aDiff]

{- | Similar to 'decodeValidation', but takes a path to a file with textual @TOML@
values from which it decodes them with the provided codec.

@since 1.3.0.0
-}
decodeFileValidation
    :: forall a m . (MonadIO m)
    => TomlCodec a
    -> FilePath
    -> m (Validation [TomlDecodeError] a)
decodeFileValidation codec = fmap (decodeValidation codec) . liftIO . TIO.readFile

{- | Similar to 'decode', but takes a path to a file with textual @TOML@
values from which it decodes them with the provided codec.

@since 1.3.0.0
-}
decodeFileEither
    :: forall a m . (MonadIO m)
    => TomlCodec a
    -> FilePath
    -> m (Either [TomlDecodeError] a)
decodeFileEither codec = fmap validationToEither . decodeFileValidation codec

{- | Similar to 'decodeExact', but takes a path to a file with textual @TOML@
values from which it decodes them with the provided codec.

@since 1.3.2.0
-}
decodeFileExact
    :: forall a m . (MonadIO m)
    => TomlCodec a
    -> FilePath
    -> m (Either [TomlDecodeError] a)
decodeFileExact codec = fmap (decodeExact codec) . liftIO . TIO.readFile

{- | Similar to 'decodeFileEither', throws 'LoadTomlException' in case of parse
errors ('TomlDecodeError').

@since 0.3.1
-}
decodeFile :: forall a m . (MonadIO m) => TomlCodec a -> FilePath -> m a
decodeFile codec filePath = decodeFileEither codec filePath >>= errorWhenLeft
  where
    errorWhenLeft :: Either [TomlDecodeError] a -> m a
    errorWhenLeft (Left errs) =
        liftIO
        $ throwIO
        $ LoadTomlException filePath
        $ prettyTomlDecodeErrors errs
    errorWhenLeft (Right pc) = pure pc

{- | Convert data type to the textual representation of @TOML@ values.

@since 0.0.0
-}
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
runTomlCodec :: TomlCodec a -> TOML -> Validation [TomlDecodeError] a
runTomlCodec = codecRead

-- | Runs 'codecWrite' of 'TomlCodec' and returns intermediate TOML AST.
execTomlCodec :: TomlCodec a -> a -> TOML
execTomlCodec codec obj = snd $ unTomlState (codecWrite codec obj) mempty
