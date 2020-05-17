{-# LANGUAGE GADTs #-}

{- |
Copyright: (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Implementations of 'BiMap' for specific Haskell types and TOML
values. Usually, you use codecs from the "Toml.Codec.Combinator"
module. You may need to use these 'BiMap's instead of codecs in the
following situations:

1. When using 'Toml.Codec.Combinator.List.arrayOf' combinator (or similar).
2. When using 'Toml.Codec.Combinator.Map.tableMap' combinator (for keys).
3. When implementing custom 'BiMap' for your types.

@since 1.3.0.0
-}

module Toml.Codec.BiMap.Conversion
    ( -- * Primitive
      -- ** Boolean
      _Bool
      -- ** Integral
    , _Int
    , _Word
    , _Word8
    , _Integer
    , _Natural
      -- ** Floating
    , _Double
    , _Float
      -- ** Text
    , _Text
    , _LText
    , _ByteString
    , _LByteString
    , _String

      -- * Time
    , _ZonedTime
    , _LocalTime
    , _Day
    , _TimeOfDay

      -- * Arrays
    , _Array
    , _NonEmpty
    , _Set
    , _HashSet
    , _IntSet
    , _ByteStringArray
    , _LByteStringArray

      -- * Coerce
    , _Coerce

      -- * Custom
    , _EnumBounded
    , _Read
    , _TextBy
    , _Validate

      -- * 'Key's
    , _KeyText
    , _KeyString
    , _KeyInt

      -- * General purpose
    , _Just
    , _Left
    , _Right

      -- * Internal helpers
    , _LTextText
    , _NaturalInteger
    , _NonEmptyList
    , _StringText
    , _ReadString
    , _BoundedInteger
    , _EnumBoundedText
    , _ByteStringText
    , _LByteStringText
    ) where

import Control.Category ((>>>))
import Control.Monad ((>=>))
import Data.Bifunctor (bimap, first)
import Data.ByteString (ByteString)
import Data.Coerce (Coercible, coerce)
import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.Text (Text)
import Data.Time (Day, LocalTime, TimeOfDay, ZonedTime)
import Data.Word (Word8)
import Numeric.Natural (Natural)
import Text.Read (readEither)

import Toml.Codec.BiMap (BiMap (..), TomlBiMap, TomlBiMapError (..), iso, mkAnyValueBiMap, prism,
                         tShow, wrongConstructor)
import Toml.Parser.Key (keyP)
import Toml.Type.AnyValue (AnyValue (..), applyAsToAny, matchBool, matchDay, matchDouble,
                           matchHours, matchInteger, matchLocal, matchText, matchZoned,
                           mkMatchError, toMArray)
import Toml.Type.Key (Key (..))
import Toml.Type.Printer (prettyKey)
import Toml.Type.Value (TValue (..), Value (..))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashSet as HS
import qualified Data.IntSet as IS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import qualified Toml.Parser.Core as P (errorBundlePretty, parse)

----------------------------------------------------------------------------
-- Primitive
----------------------------------------------------------------------------

{- | 'Prelude.Bool' 'BiMap' for 'AnyValue'. Usually used as the
'Toml.Codec.Combinator.Primitive.bool' combinator.

@since 0.4.0
-}
_Bool :: TomlBiMap Bool AnyValue
_Bool = mkAnyValueBiMap matchBool Bool
{-# INLINE _Bool #-}

{- | 'Prelude.Integer' 'BiMap' for 'AnyValue'. Usually used as the
'Toml.Codec.Combinator.Primitive.integer' combinator.

@since 0.4.0
-}
_Integer :: TomlBiMap Integer AnyValue
_Integer = mkAnyValueBiMap matchInteger Integer
{-# INLINE _Integer #-}

{- | 'Prelude.Double' 'BiMap' for 'AnyValue'. Usually used as the
'Toml.Codec.Combinator.Primitive.double' combinator.

@since 0.4.0
-}
_Double :: TomlBiMap Double AnyValue
_Double = mkAnyValueBiMap matchDouble Double
{-# INLINE _Double #-}

{- | 'Data.Text.Text' 'BiMap' for 'AnyValue'. Usually used as the
'Toml.Codec.Combinator.Primitive.text' combinator.

@since 0.4.0
-}
_Text :: TomlBiMap Text AnyValue
_Text = mkAnyValueBiMap matchText Text
{-# INLINE _Text #-}

{- | Helper bimap for 'Data.Text.Lazy.Text' and 'Data.Text.Text'.

@since 1.0.0
-}
_LTextText :: BiMap e TL.Text Text
_LTextText = iso TL.toStrict TL.fromStrict
{-# INLINE _LTextText #-}

{- | 'Data.Text.Lazy.Text' 'BiMap' for 'AnyValue'. Usually used as the
'Toml.Codec.Combinator.Primitive.lazyText' combinator.

@since 1.0.0
-}
_LText :: TomlBiMap TL.Text AnyValue
_LText = _LTextText >>> _Text
{-# INLINE _LText #-}

{- | 'Data.Time.ZonedTime' bimap for 'AnyValue'. Usually used as the
'Toml.Codec.Combinator.Time.zonedTime' combinator.

@since 0.5.0
-}
_ZonedTime :: TomlBiMap ZonedTime AnyValue
_ZonedTime = mkAnyValueBiMap matchZoned Zoned
{-# INLINE _ZonedTime #-}

{- | 'Data.Time.LocalTime' bimap for 'AnyValue'. Usually used as the
'Toml.Codec.Combinator.Time.localTime' combinator.

@since 0.5.0
-}
_LocalTime :: TomlBiMap LocalTime AnyValue
_LocalTime = mkAnyValueBiMap matchLocal Local
{-# INLINE _LocalTime #-}

{- | 'Data.Time.Day' 'BiMap' for 'AnyValue'. Usually used as the
'Toml.Codec.Combinator.Time.day' combinator.

@since 0.5.0
-}
_Day :: TomlBiMap Day AnyValue
_Day = mkAnyValueBiMap matchDay Day
{-# INLINE _Day #-}

{- | 'Data.Time.TimeOfDay' 'BiMap' for 'AnyValue'. Usually used as the
'Toml.Codec.Combinator.Time.timeOfDay' combinator.

@since 0.5.0
-}
_TimeOfDay :: TomlBiMap TimeOfDay AnyValue
_TimeOfDay = mkAnyValueBiMap matchHours Hours
{-# INLINE _TimeOfDay #-}

{- | Helper 'BiMap' for 'String' and 'Data.Text.Text'.

@since 0.4.0
-}
_StringText :: BiMap e String Text
_StringText = iso T.pack T.unpack
{-# INLINE _StringText #-}

{- | 'String' 'BiMap' for 'AnyValue'. Usually used as the
'Toml.Codec.Combinator.Primitive.string' combinator.

@since 0.4.0
-}
_String :: TomlBiMap String AnyValue
_String = _StringText >>> _Text
{-# INLINE _String #-}

{- | Helper 'BiMap' for 'Natural' and 'Prelude.Integer'.

@since 0.5.0
-}
_NaturalInteger :: TomlBiMap Natural Integer
_NaturalInteger = BiMap (Right . toInteger) eitherInteger
  where
    eitherInteger :: Integer -> Either TomlBiMapError Natural
    eitherInteger n
      | n < 0     = Left $ ArbitraryError $ "Value is below zero, but expected Natural: " <> tShow n
      | otherwise = Right (fromIntegral n)

{- | 'Natural' 'BiMap' for 'AnyValue'. Usually used as the
'Toml.Codec.Combinator.Primitive.natural' combinator.

@since 0.5.0
-}
_Natural :: TomlBiMap Natural AnyValue
_Natural = _NaturalInteger >>> _Integer
{-# INLINE _Natural #-}

{- | Helper 'BiMap' for 'Prelude.Integer' and integral, bounded values.

@since 0.5.0
-}
_BoundedInteger :: (Integral a, Bounded a, Show a) => TomlBiMap a Integer
_BoundedInteger = BiMap (Right . toInteger) eitherBounded
  where
    eitherBounded :: forall a. (Integral a, Bounded a, Show a) => Integer -> Either TomlBiMapError a
    eitherBounded n
      | n < toInteger (minBound @a) =
         let msg = "Value " <> tShow n <> " is less than minBound: " <> tShow (minBound @a)
         in Left $ ArbitraryError msg
      | n > toInteger (maxBound @a) =
         let msg = "Value " <> tShow n <> " is greater than maxBound: " <> tShow (maxBound @a)
         in Left $ ArbitraryError msg
      | otherwise = Right (fromIntegral n)


{- | 'Word' 'BiMap' for 'AnyValue'. Usually used as the
'Toml.Codec.Combinator.Primitive.word' combinator.

@since 0.5.0
-}
_Word :: TomlBiMap Word AnyValue
_Word = _BoundedInteger >>> _Integer
{-# INLINE _Word #-}

{- | 'Word8' 'BiMap' for 'AnyValue'. Usually used as the
'Toml.Codec.Combinator.Primitive.word8' combinator.

@since 1.2.0.0
-}
_Word8 :: TomlBiMap Word8 AnyValue
_Word8 = _BoundedInteger >>> _Integer
{-# INLINE _Word8 #-}

{- | 'Int' 'BiMap' for 'AnyValue'. Usually used as the
'Toml.Codec.Combinator.Primitive.int' combinator.

@since 0.5.0
-}
_Int :: TomlBiMap Int AnyValue
_Int = _BoundedInteger >>> _Integer
{-# INLINE _Int #-}

{- | 'Float' 'BiMap' for 'AnyValue'. Usually used as the
'Toml.Codec.Combinator.Primitive.float' combinator.

@since 0.5.0
-}
_Float :: TomlBiMap Float AnyValue
_Float = iso realToFrac realToFrac >>> _Double
{-# INLINE _Float #-}

{- | Helper 'BiMap' for 'Data.Text.Text' and strict 'ByteString'

@since 0.5.0
-}
_ByteStringText :: TomlBiMap ByteString Text
_ByteStringText = prism T.encodeUtf8 eitherText
  where
    eitherText :: ByteString -> Either TomlBiMapError Text
    eitherText = either (\err -> Left $ ArbitraryError $ tShow err) Right . T.decodeUtf8'
{-# INLINE _ByteStringText #-}

{- | UTF-8 encoded 'ByteString' 'BiMap' for 'AnyValue'.
Usually used as the 'Toml.Codec.Combinator.Primitive.byteString' combinator.

@since 0.5.0
-}
_ByteString :: TomlBiMap ByteString AnyValue
_ByteString = _ByteStringText >>> _Text
{-# INLINE _ByteString #-}

{- | Helper 'BiMap' for 'Data.Text.Text' and lazy 'BL.ByteString'.

@since 0.5.0
-}
_LByteStringText :: TomlBiMap BL.ByteString Text
_LByteStringText = prism (TL.encodeUtf8 . TL.fromStrict) eitherText
  where
    eitherText :: BL.ByteString -> Either TomlBiMapError Text
    eitherText = bimap (ArbitraryError . tShow) TL.toStrict . TL.decodeUtf8'
{-# INLINE _LByteStringText #-}

{- | UTF-8 encoded lazy 'BL.ByteString' 'BiMap' for 'AnyValue'.
Usually used as the 'Toml.Codec.Combinator.Primitive.lazyByteString' combinator.

@since 0.5.0
-}
_LByteString :: TomlBiMap BL.ByteString AnyValue
_LByteString = _LByteStringText >>> _Text
{-# INLINE _LByteString #-}

----------------------------------------------------------------------------
-- Array
----------------------------------------------------------------------------

{- | 'ByteString' 'BiMap' for 'AnyValue' encoded as a list of bytes
(non-negative integers between 0 and 255). Usually used as the
'Toml.Codec.Combinator.Primitive.byteStringArray' combinator.

@since 1.2.0.0
-}
_ByteStringArray :: TomlBiMap ByteString AnyValue
_ByteStringArray = iso BS.unpack BS.pack >>> _Array _Word8
{-# INLINE _ByteStringArray #-}

{- | Lazy 'ByteString' 'BiMap' for 'AnyValue' encoded as a list of
bytes (non-negative integers between 0 and 255). Usually used as
'Toml.Codec.Combinator.Primitive.lazyByteStringArray' combinator.

@since 1.2.0.0
-}
_LByteStringArray :: TomlBiMap BL.ByteString AnyValue
_LByteStringArray = iso BL.unpack BL.pack >>>  _Array _Word8
{-# INLINE _LByteStringArray #-}

{- | Takes a 'BiMap' of a value and returns a 'BiMap' for a list of values and 'AnyValue'
as an array. Usually used as the 'Toml.Codec.Combinator.List.arrayOf' combinator.

@since 0.4.0
-}
_Array :: forall a . TomlBiMap a AnyValue -> TomlBiMap [a] AnyValue
_Array elementBimap = BiMap toAnyValue fromAnyValue
  where
    toAnyValue :: [a] -> Either TomlBiMapError AnyValue
    toAnyValue = mapM (forward elementBimap) >=> bimap WrongValue AnyValue . toMArray

    fromAnyValue :: AnyValue -> Either TomlBiMapError [a]
    fromAnyValue (AnyValue v) = matchElements (backward elementBimap) v

    -- can't reuse matchArray here :(
    matchElements :: (AnyValue -> Either TomlBiMapError a) -> Value t -> Either TomlBiMapError [a]
    matchElements match (Array a) = mapM (applyAsToAny match) a
    matchElements _ val           = first WrongValue $ mkMatchError TArray val

{- | Takes a 'BiMap' of a value and returns a 'BiMap' for a 'NonEmpty'
list of values and 'AnyValue' as an array. Usually used as the
'Toml.Codec.Combinator.List.arrayNonEmptyOf' combinator.

@since 0.5.0
-}
_NonEmpty :: TomlBiMap a AnyValue -> TomlBiMap (NE.NonEmpty a) AnyValue
_NonEmpty bi = _NonEmptyList >>> _Array bi
{-# INLINE _NonEmpty #-}

{- | Helper 'BiMap' for lists and 'NE.NonEmpty'.

@since 1.3.0.0
-}
_NonEmptyList :: TomlBiMap (NE.NonEmpty a) [a]
_NonEmptyList = BiMap
    { forward  = Right . NE.toList
    , backward = maybe (Left $ ArbitraryError "Empty array list, but expected NonEmpty") Right . NE.nonEmpty
    }
{-# INLINE _NonEmptyList #-}

{- | Takes a 'BiMap' of a value and returns a 'BiMap' for a 'Set' of
values and 'AnyValue' as an array. Usually used as the
'Toml.Codec.Combinator.Set.arraySetOf' combinator.

@since 0.5.0
-}
_Set :: (Ord a) => TomlBiMap a AnyValue -> TomlBiMap (S.Set a) AnyValue
_Set bi = iso S.toList S.fromList >>> _Array bi
{-# INLINE _Set #-}

{- | Takes a 'BiMap' of a value and returns a 'BiMap' for a 'HashSet' of
values and 'AnyValue' as an array. Usually used as the
'Toml.Codec.Combinator.Set.arrayHashSetOf' combinator.

@since 0.5.0
-}
_HashSet :: (Eq a, Hashable a) => TomlBiMap a AnyValue -> TomlBiMap (HS.HashSet a) AnyValue
_HashSet bi = iso HS.toList HS.fromList >>> _Array bi
{-# INLINE _HashSet #-}

{- | 'IS.IntSet' bimap for 'AnyValue'. Usually used as the
'Toml.Codec.Combinator.Set.arrayIntSet' combinator.

@since 0.5.0
-}
_IntSet :: TomlBiMap IS.IntSet AnyValue
_IntSet = iso IS.toList IS.fromList >>> _Array _Int
{-# INLINE _IntSet #-}

----------------------------------------------------------------------------
-- Coerce
----------------------------------------------------------------------------

{- | 'BiMap' for 'Coercible' values. It takes a 'TomlBiMap'
for @a@ type and returns a 'TomlBiMap' @b@ if these types are coercible.

It is supposed to be used to ease the work with @newtypes@.

E.g.

@
__newtype__ Foo = Foo
    { unFoo :: 'Int'
    }

fooBiMap :: 'TomlBiMap' Foo 'AnyValue'
fooBiMap = '_Coerce' '_Int'
@

@since 1.3.0.0
-}
_Coerce :: (Coercible a b) => TomlBiMap a AnyValue -> TomlBiMap b AnyValue
_Coerce = coerce
{-# INLINE _Coerce #-}

----------------------------------------------------------------------------
-- Custom
----------------------------------------------------------------------------

{- | Helper 'BiMap' for 'String' and types with 'Read' and 'Show' instances.

@since 0.5.0
-}
_ReadString :: (Show a, Read a) => TomlBiMap a String
_ReadString = BiMap (Right . show) (first (ArbitraryError . T.pack) . readEither)
{-# INLINE _ReadString #-}

{- | 'BiMap' for 'AnyValue' and values with a 'Read' and 'Show' instances.
Usually used as the 'Toml.Codec.Combinator.Custom.read' combinator.

@since 0.5.0
-}
_Read :: (Show a, Read a) => TomlBiMap a AnyValue
_Read = _ReadString >>> _String
{-# INLINE _Read #-}

{- | Creates 'BiMap' for 'Data.Text.Text' to 'AnyValue' with custom functions.
Usually used as the 'Toml.Codec.Combinator.Custom.textBy' combinator.

@since 0.5.0
-}
_TextBy
    :: forall a .
       (a -> Text)              -- ^ @show@ function for @a@
    -> (Text -> Either Text a)  -- ^ Parser of @a@ from 'Data.Text.Text'
    -> TomlBiMap a AnyValue
_TextBy toText parseText = BiMap toAnyValue fromAnyValue
  where
    toAnyValue :: a -> Either TomlBiMapError AnyValue
    toAnyValue = Right . AnyValue . Text . toText

    fromAnyValue :: AnyValue -> Either TomlBiMapError a
    fromAnyValue (AnyValue v) =
        first WrongValue (matchText v) >>= first ArbitraryError . parseText

{- | By the given 'BiMap' validates it with the given predicate that returns
'Either' the value, if the validation is successful, or the 'Text' of the error
that should be returned in case of validation failure.

Usually used as the 'Toml.Codec.Combinator.Custom.validate' or
'Toml.Codec.Combinator.Custom.validateIf' combinator.

@since 1.3.0.0
-}
_Validate :: forall a . (a -> Either Text a) -> TomlBiMap a AnyValue -> TomlBiMap a AnyValue
_Validate p BiMap{..} = BiMap forward backwardWithValidation
  where
    backwardWithValidation :: AnyValue -> Either TomlBiMapError a
    backwardWithValidation anyVal = backward anyVal >>= first ArbitraryError . p

{- | Helper 'BiMap' for '_EnumBounded' and 'Data.Text.Text'.

@since 1.1.0.0
-}
_EnumBoundedText :: forall a. (Show a, Enum a, Bounded a) => TomlBiMap a Text
_EnumBoundedText = BiMap
    { forward  = Right . tShow
    , backward = toEnumBounded
    }
  where
    toEnumBounded :: Text -> Either TomlBiMapError a
    toEnumBounded value = case M.lookup value enumOptions of
        Just a  -> Right a
        Nothing ->
            let msg = "Value is '" <> value <> "' but expected one of: " <> T.intercalate ", " options
            in Left (ArbitraryError msg)
      where
        enumOptions :: Map Text a
        enumOptions = M.fromList $ zip options enums
        options  = fmap tShow enums
        enums = [minBound @a .. maxBound @a]

{- | 'BiMap' for nullary sum data types (enumerations) with 'Show',
'Enum' and 'Bounded' instances. Usually used as the
'Toml.Codec.Combinator.Custom.enumBounded' combinator.

@since 1.1.0.0
-}
_EnumBounded :: (Show a, Enum a, Bounded a) => TomlBiMap a AnyValue
_EnumBounded = _EnumBoundedText >>> _Text
{-# INLINE _EnumBounded #-}

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

{- | Bidirectional converter between 'Key' and
'Data.Text.Text'. Usually used as an argument for
'Toml.Codec.Combinator.Map.tableMap'.

@since 1.3.0.0
-}
_KeyText :: TomlBiMap Key Text
_KeyText = BiMap
    { forward = Right . prettyKey
    , backward = textToKey
    }

{- | Bidirectional converter between 'Key' and 'String'. Usually used
as an argument for 'Toml.Codec.Combinator.Map.tableMap'.

@since 1.3.0.0
-}
_KeyString :: TomlBiMap Key String
_KeyString = BiMap
    { forward = Right . T.unpack . prettyKey
    , backward = textToKey . T.pack
    }

{- | Bidirectional converter between 'Key' and 'Int'. Usually used
as an argument for 'Toml.Codec.Combinator.Map.tableIntMap'.

@since 1.3.0.0
-}
_KeyInt :: TomlBiMap Key Int
_KeyInt = BiMap
    { forward = first (ArbitraryError . T.pack) . readEither . T.unpack . prettyKey
    , backward = textToKey . tShow
    }

textToKey :: Text -> Either TomlBiMapError Key
textToKey t = case P.parse keyP "" t of
    Left err  -> Left $ ArbitraryError $ T.pack $ P.errorBundlePretty err
    Right key -> Right key

----------------------------------------------------------------------------
-- General purpose bimaps
----------------------------------------------------------------------------

{- | 'BiMap' for 'Either' and its 'Left' part.

@since 0.4.0
-}
_Left :: (Show l, Show r) => TomlBiMap (Either l r) l
_Left = prism Left $ \case
    Left l -> Right l
    x -> wrongConstructor "Left" x

{- | 'BiMap' for 'Either' and its 'Right' part.

@since 0.4.0
-}
_Right :: (Show l, Show r) => TomlBiMap (Either l r) r
_Right = prism Right $ \case
    Right r -> Right r
    x -> wrongConstructor "Right" x

{- | 'BiMap' for 'Maybe' and its 'Just' part.

@since 0.5.0
-}
_Just :: Show r => TomlBiMap (Maybe r) r
_Just = prism Just $ \case
    Just r -> Right r
    x -> wrongConstructor "Just" x
