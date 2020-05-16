{- |
Copyright: (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Contains TOML-specific combinators for converting between TOML and user data
types.

@since 1.3.0.0
-}

module Toml.Codec.Combinator.Primitive
    ( -- * Boolean
      bool
      -- * Integral numbers
    , integer
    , int
    , natural
    , word
    , word8
      -- * Floating point numbers
    , double
    , float
      -- * Text types
    , string
    , text
    , lazyText
    , byteString
    , lazyByteString
    , byteStringArray
    , lazyByteStringArray
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word (Word8)
import Numeric.Natural (Natural)

import Toml.Codec.BiMap.Conversion (_Bool, _ByteString, _ByteStringArray, _Double, _Float, _Int,
                                    _Integer, _LByteString, _LByteStringArray, _LText, _Natural,
                                    _String, _Text, _Word, _Word8)
import Toml.Codec.Combinator.Common (match)
import Toml.Codec.Types (TomlCodec)
import Toml.Type.Key (Key)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as L


-- | Codec for boolean values.
bool :: Key -> TomlCodec Bool
bool = match _Bool
{-# INLINE bool #-}

-- | Codec for integer values.
integer :: Key -> TomlCodec Integer
integer = match _Integer
{-# INLINE integer #-}

-- | Codec for integer values.
int :: Key -> TomlCodec Int
int = match _Int
{-# INLINE int #-}

-- | Codec for natural values.
natural :: Key -> TomlCodec Natural
natural = match _Natural
{-# INLINE natural #-}

-- | Codec for word values.
word :: Key -> TomlCodec Word
word = match _Word
{-# INLINE word #-}

{- | Codec for word8 values.

@since 1.2.0.0
-}
word8 :: Key -> TomlCodec Word8
word8 = match _Word8
{-# INLINE word8 #-}

-- | Codec for floating point values with double precision.
double :: Key -> TomlCodec Double
double = match _Double
{-# INLINE double #-}

-- | Codec for floating point values.
float :: Key -> TomlCodec Float
float = match _Float
{-# INLINE float #-}

-- | Codec for string values.
string :: Key -> TomlCodec String
string = match _String
{-# INLINE string #-}

-- | Codec for text values.
text :: Key -> TomlCodec Text
text = match _Text
{-# INLINE text #-}

-- | Codec for lazy text values.
lazyText :: Key -> TomlCodec L.Text
lazyText = match _LText
{-# INLINE lazyText #-}

-- | Codec for text values as 'ByteString'.
byteString :: Key -> TomlCodec ByteString
byteString = match _ByteString
{-# INLINE byteString #-}

-- | Codec for text values as 'BL.ByteString'.
lazyByteString :: Key -> TomlCodec BL.ByteString
lazyByteString = match _LByteString
{-# INLINE lazyByteString #-}

{- | Codec for positive integer array values as 'ByteString'.

@since 1.2.0.0
-}
byteStringArray :: Key -> TomlCodec ByteString
byteStringArray = match _ByteStringArray
{-# INLINE byteStringArray #-}

{- | Codec for positive integer array values as lazy 'ByteString'.

@since 1.2.0.0
-}
lazyByteStringArray :: Key -> TomlCodec BL.ByteString
lazyByteStringArray = match _LByteStringArray
{-# INLINE lazyByteStringArray #-}
