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

      -- * Validation
    , validate
    , validateIf
    ) where

import Prelude hiding (read)

import Data.Text (Text)

import Toml.Codec.BiMap (TomlBiMap)
import Toml.Codec.BiMap.Conversion (_EnumBounded, _Read, _TextBy, _Validate)
import Toml.Codec.Combinator.Common (match)
import Toml.Codec.Types (TomlCodec)
import Toml.Type.AnyValue (AnyValue)
import Toml.Type.Key (Key, keyToText)


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

{- | Codec that checks the 'BiMap' on the given predicate.
The predicate function returns the value, if the validation is successful, or
the 'Text' of the error that should be returned in case of validation failure.

__Example:__

Let's imagine that we want to have the list in @TOML@ that could only
have even 'Int's inside. In this case, you can write the following codec:

@
allEven :: ['Int'] -> 'Either' 'Text' ['Int']
allEven xs =
    if all even xs
    then 'Right' xs
    else 'Left' "This is wrong, I asked you for even only :("

allEvenCodec :: 'TomlCodec' ['Int']
allEvenCodec = Toml.'validate' allEven (Toml._Array Toml._Int) \"myEvenList\"
@

Then in your @TOML@ file you can have:

@
myEvenList = [2, 4, 6]
@

But the following one will lead to the error:

@
myEvenList = [1, 2, 3]
@

@
tomland decode error:  This is wrong, I asked you for even only :(
@

@since 1.3.0.0
-}
validate :: (a -> Either Text a) -> TomlBiMap a AnyValue -> Key -> TomlCodec a
validate p biMap = match (_Validate p biMap)
{-# INLINE validate #-}

{- | Similar to 'validate' but takes the predicate that returnes 'Bool'.
The error in case of the validation failure looks like this:

@
Value does not pass the validation for key: KEY_NAME
@

__Example:__

Let's imagine that we want to have the 'Text' field in @TOML@ that could only
have 3 chars in it. In this case, you can write the following codec:

@
my3charTextCodec :: TomlCodec Text
my3CharTextCodec = Toml.'validateIf' ((==) 3 . Text.length) Toml._Text "myKeyName"
@

The in your @TOML@ file you can have:

@
myKeyName = "abc"
@

But the following one will lead to the error:

@
myKeyName = "I have more than enough"
@

@
tomland decode error:  Value does not pass the validation for key: myKeyName
@

@since 1.3.0.0
-}
validateIf :: forall a . (a -> Bool) -> TomlBiMap a AnyValue -> Key -> TomlCodec a
validateIf p biMap k = validate validateEither biMap k
  where
    validateEither :: a -> Either Text a
    validateEither a =
        if p a
        then Right a
        else Left $ "Value does not pass the validation for key: " <> keyToText k
