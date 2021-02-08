{- |
Module                  : Toml.Codec.Combinator.Custom
Copyright               : (c) 2018-2021 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Contains TOML-specific custom combinators for converting between TOML and
special user data types.

See examples below of the situations you may need the following combinators.

@since 1.3.0.0
-}

module Toml.Codec.Combinator.Custom
    ( -- * 'Text' combinators
      textBy
    , read
    , enumBounded
    , hardcoded

      -- * Validation
    , validate
    , validateIf
    ) where

import Prelude hiding (read)

import Control.Category ((>>>))
import Data.Text (Text)

import Toml.Codec.BiMap (TomlBiMap)
import Toml.Codec.BiMap.Conversion (_EnumBounded, _Read, _TextBy, _Validate, _Hardcoded)
import Toml.Codec.Combinator.Common (match)
import Toml.Codec.Types (TomlCodec)
import Toml.Type.AnyValue (AnyValue)
import Toml.Type.Key (Key)
import Toml.Type.Printer (prettyKey)


{- | Codec for text values with custom error messages for parsing.

__Example:__

We have the following type that represents the image format:

@
__data__ Format
    = Jpeg
    | Png
    | Gif
    __deriving__ ('Show', 'Read', 'Enum')
@

But we want to be able to decode and encode this data type through the custom
text representation, that can be formilised in the following functions:

@
showFormat :: Format -> 'Text'
showFormat = \case
    Jpeg -> ".jpeg"
    Png  -> ".png"
    Gif  -> ".gif"

parseFormat :: 'Text' -> 'Either' 'Text' Format
parseFormat = __\case__
    ".jpeg" -> 'Right' Jpeg
    ".png"  -> 'Right' Png
    ".gif"  -> 'Right' Gif
    other   -> 'Left' $ "Unsupported format: " <> other
@

To write the codec for @Format@ data type using the above rules we can use
'textBy' combinator:

@
formatCodec :: 'Key' -> 'TomlCodec' Format
formatCodec = 'textBy' showFormat parseFormat
@

And now with the @formatCodec "foo"@ we can have the following line in our
@TOML@ perfectly encoded:

@
foo = ".gif"
@

But the @foo = "jif"@ will lead to the following error:

@
tomland decode error:  Unsupported format: jif
@

@since 1.0.0
-}
textBy :: (a -> Text) -> (Text -> Either Text a) -> Key -> TomlCodec a
textBy to from = match (_TextBy to from)
{-# INLINE textBy #-}


{- | Codec for values with a 'Read' and 'Show' instance.

__Example:__

We have the following type that represents the image format:

@
__data__ Format
    = Jpeg
    | Png
    | Gif
    deriving (Show, Read, Enum)
@

And we want to be able to decode and encode this data type through the 'Show'
and 'Read' instances.

To write the codec for @Format@ data type using the existing instances we can
use 'read' combinator. And now with the @Toml.'read' "foo"@ we can have the
following line in our @TOML@ perfectly encoded:

@
foo = "Gif"
@

But the @foo = ".gif"@ will lead to the following error:

@
tomland decode error:  Prelude.read: no parse
@

@since 0.5.0
-}
read :: (Show a, Read a) => Key -> TomlCodec a
read = match _Read
{-# INLINE read #-}

{- | Codec for general nullary sum data types with a 'Bounded', 'Enum', and
'Show' instance. This codec is similar to 'read' but provides much better error
messages than 'read' for nullary sum types.

E.g. for the same @Format@ example from 'read' function, but with the
@Toml.'enumBounded' "foo"@ codec the error for @foo = \"Jif\"@ in the @TOML@ file
will look like this:

@
tomland decode error:  Value is 'Jif' but expected one of: Jpeg, Png, Gif
@

@since 1.1.0.0
-}
enumBounded :: (Bounded a, Enum a, Show a) => Key -> TomlCodec a
enumBounded = match _EnumBounded
{-# INLINE enumBounded #-}

{- | Codec for hardcoded provided values and its 'BiMap'.

If you want to decode a single key-value pair where only one value is allowed.
Like in the example below:

@
scope = "all"
@

To decode that you could use the following function:

@
Toml.hardcoded "all" Toml._Text "scope"
@

in case if the value in @TOML@ is not the same as hardcoded, you will get the following error:

@
tomland decode error:  BiMap error in key 'scope' : Value '"foo"' doesn't align with the hardcoded value '"all"'
@

@since x.x.x.x
-}
hardcoded :: (Show a, Eq a) => a -> TomlBiMap a AnyValue -> Key -> TomlCodec a
hardcoded a aBm = match (_Hardcoded a >>> aBm)

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
        else Left $ "Value does not pass the validation for key: " <> prettyKey k
