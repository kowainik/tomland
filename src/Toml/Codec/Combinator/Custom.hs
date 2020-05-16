{- |
Copyright: (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Contains TOML-specific combinators for converting between TOML and user data
types.

@since 1.3.0.0
-}

module Toml.Codec.Combinator.Custom
    ( -- * Additional codecs for custom types
      textBy
    , read
    , enumBounded
    ) where

import Prelude hiding (read)

import Data.Text (Text)

import Toml.Codec.BiMap.Conversion (_EnumBounded, _Read, _TextBy)
import Toml.Codec.Combinator.Common (match)
import Toml.Codec.Types (TomlCodec)
import Toml.Type.Key (Key)


-- | Codec for text values with custom error messages for parsing.
textBy :: (a -> Text) -> (Text -> Either Text a) -> Key -> TomlCodec a
textBy to from = match (_TextBy to from)
{-# INLINE textBy #-}


-- | Codec for values with a 'Read' and 'Show' instance.
read :: (Show a, Read a) => Key -> TomlCodec a
read = match _Read
{-# INLINE read #-}

{- | Codec for general nullary sum data types with a 'Bounded', 'Enum', and
'Show' instance. This codec provides much better error messages than 'read' for
nullary sum types.

@since 1.1.1.0
-}
enumBounded :: (Bounded a, Enum a, Show a) => Key -> TomlCodec a
enumBounded = match _EnumBounded
{-# INLINE enumBounded #-}
