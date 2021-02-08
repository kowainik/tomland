{- |
Module                  : Toml.Codec.Combinator.Primitive
Copyright               : (c) 2018-2021 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

TOML-specific combinators for converting between TOML and Haskell primitive
types, e.g. 'Int', 'ByteString'.

For the overall picture you can see how different types are represented by
codecs in the following table:

+---------------------------+-----------------+---------------------------+
|       Haskell Type        |     @TOML@      |      'TomlCodec'          |
+===========================+=================+===========================+
| 'Bool'                    | @a = true@      | 'bool' "a"                |
+---------------------------+-----------------+---------------------------+
| 'Integer'                 | @a = 100@       | 'integer' "a"             |
+---------------------------+-----------------+---------------------------+
| 'Int'                     | @a = -42@       | 'int' "a"                 |
+---------------------------+-----------------+---------------------------+
| 'Natural'                 | @a = 11@        | 'natural' "a"             |
+---------------------------+-----------------+---------------------------+
| 'Word'                    | @a = 1@         | 'word' "a"                |
+---------------------------+-----------------+---------------------------+
| 'Word8'                   | @a = 1@         | 'word8' "a"               |
+---------------------------+-----------------+---------------------------+
| 'Double'                  | @a = 36.6@      | 'double' "a"              |
+---------------------------+-----------------+---------------------------+
| 'Float'                   | @a = -100.09@   | 'float' "a"               |
+---------------------------+-----------------+---------------------------+
| 'String'                  | @a = \"Hello\"@ | 'string' "a"              |
+---------------------------+-----------------+---------------------------+
| 'Text'                    | @a = \"Hello\"@ | 'text' "a"                |
+---------------------------+-----------------+---------------------------+
| Lazy'L.Text'              | @a = \"Hey\"@   | 'lazyText' "a"            |
+---------------------------+-----------------+---------------------------+
| 'ByteString'              | @a = \"Hello\"@ | 'byteString' "a"          |
+---------------------------+-----------------+---------------------------+
| Lazy'BL.ByteString'       | @a = \"Hey\"@   | 'lazyByteString' "a"      |
+---------------------------+-----------------+---------------------------+
| 'ByteString' as Array     | @a = [10, 15]@  | 'byteStringArray' "a"     |
+---------------------------+-----------------+---------------------------+
| Lazy'ByteString' as Array | @a = [15, 10]@  | 'lazyByteStringArray' "a" |
+---------------------------+-----------------+---------------------------+

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


{- | Codec for boolean values.

@since 0.0.0
-}
bool :: Key -> TomlCodec Bool
bool = match _Bool
{-# INLINE bool #-}

{- | Codec for integer values.

@since 0.1.0
-}
integer :: Key -> TomlCodec Integer
integer = match _Integer
{-# INLINE integer #-}

{- | Codec for integer values.

@since 0.0.0
-}
int :: Key -> TomlCodec Int
int = match _Int
{-# INLINE int #-}

{- | Codec for natural values.

@since 0.5.0
-}
natural :: Key -> TomlCodec Natural
natural = match _Natural
{-# INLINE natural #-}

{- | Codec for word values.

@since 0.5.0
-}
word :: Key -> TomlCodec Word
word = match _Word
{-# INLINE word #-}

{- | Codec for word8 values.

@since 1.2.0.0
-}
word8 :: Key -> TomlCodec Word8
word8 = match _Word8
{-# INLINE word8 #-}

{- | Codec for floating point values with double precision.

@since 0.0.0
-}
double :: Key -> TomlCodec Double
double = match _Double
{-# INLINE double #-}

{- | Codec for floating point values.

@since 0.5.0
-}
float :: Key -> TomlCodec Float
float = match _Float
{-# INLINE float #-}

{- | Codec for string values.

@since 0.4.0
-}
string :: Key -> TomlCodec String
string = match _String
{-# INLINE string #-}

{- | Codec for text values.

@since 0.3.0
-}
text :: Key -> TomlCodec Text
text = match _Text
{-# INLINE text #-}

{- | Codec for lazy text values.

@since 1.0.0
-}
lazyText :: Key -> TomlCodec L.Text
lazyText = match _LText
{-# INLINE lazyText #-}

{- | Codec for text values as 'ByteString'.

@since 0.5.0
-}
byteString :: Key -> TomlCodec ByteString
byteString = match _ByteString
{-# INLINE byteString #-}

{- | Codec for text values as 'BL.ByteString'.

@since 0.5.0
-}
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
