{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TypeFamilies        #-}

{- | Implementation of tagged partial bidirectional isomorphism.
-}

module Toml.Bi.Map
       ( -- * BiMap idea
         BiMap (..)
       , TomlBiMap
       , invert
       , iso
       , prism

         -- * 'BiMap' errors for TOML
       , TomlBiMapError (..)
       , wrongConstructor
       , prettyBiMapError

         -- * Helpers for BiMap and AnyValue
       , mkAnyValueBiMap
       , _TextBy
       , _NaturalInteger
       , _StringText
       , _ReadString
       , _BoundedInteger
       , _ByteStringText
       , _LByteStringText

         -- * Some predefined bi mappings
       , _Array
       , _Bool
       , _Double
       , _Integer
       , _Text
       , _ZonedTime
       , _LocalTime
       , _Day
       , _TimeOfDay
       , _String
       , _Read
       , _Natural
       , _Word
       , _Int
       , _Float
       , _ByteString
       , _LByteString
       , _Set
       , _IntSet
       , _HashSet
       , _NonEmpty

       , _Left
       , _Right
       , _Just

         -- * Useful utility functions
       , toMArray
       ) where

import Control.Arrow ((>>>))
import Control.Monad ((>=>))

import Control.DeepSeq (NFData)
import Data.Bifunctor (bimap, first)
import Data.ByteString (ByteString)
import Data.Hashable (Hashable)
import Data.Semigroup (Semigroup (..))
import Data.Text (Text)
import Data.Time (Day, LocalTime, TimeOfDay, ZonedTime)
import Data.Word (Word)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Text.Read (readEither)

import Toml.Type (AnyValue (..), MatchError (..), TValue (..), Value (..), applyAsToAny, matchBool,
                  matchDay, matchDouble, matchHours, matchInteger, matchLocal, matchText,
                  matchZoned, mkMatchError, toMArray)

import qualified Control.Category as Cat
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashSet as HS
import qualified Data.IntSet as IS
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL


----------------------------------------------------------------------------
-- BiMap concepts and ideas
----------------------------------------------------------------------------

{- | Partial bidirectional isomorphism. @BiMap a b@ contains two function:

1. @a -> Either e b@
2. @b -> Either e a@

If you think of types as sets then this data type can be illustrated by the
following picture:

![bimap-type](https://user-images.githubusercontent.com/4276606/50770531-b6a36000-1298-11e9-9528-caae87951d2a.png)

'BiMap' also implements 'Cat.Category' typeclass. And this instance can be described
clearly by this illustration:

![bimap-cat](https://user-images.githubusercontent.com/4276606/50771234-13a01580-129b-11e9-93da-6c5dd0f7f160.png)
-}
data BiMap e a b = BiMap
    { forward  :: a -> Either e b
    , backward :: b -> Either e a
    }

instance Cat.Category (BiMap e) where
    id :: BiMap e a a
    id = BiMap Right Right

    (.) :: BiMap e b c -> BiMap e a b -> BiMap e a c
    bc . ab = BiMap
        { forward  =  forward ab >=>  forward bc
        , backward = backward bc >=> backward ab
        }

-- | Inverts bidirectional mapping.
invert :: BiMap e a b -> BiMap e b a
invert (BiMap f g) = BiMap g f

{- | Creates 'BiMap' from isomorphism. Can be used in the following way:

@
__newtype__ Even = Even Integer
__newtype__ Odd  = Odd  Integer

succEven :: Even -> Odd
succEven (Even n) = Odd (n + 1)

predOdd :: Odd -> Even
predOdd (Odd n) = Even (n - 1)

_EvenOdd :: 'BiMap' e Even Odd
_EvenOdd = 'iso' succEven predOdd
@
-}
iso :: (a -> b) -> (b -> a) -> BiMap e a b
iso f g = BiMap (Right . f) (Right . g)

{- | Creates 'BiMap' from prism-like pair of functions. This combinator can be
used to create 'BiMap' for custom data types like this:

@
__data__ User
    = Admin  Integer  -- id of admin
    | Client Text     -- name of the client
    __deriving__ (Show)

_Admin :: 'TomlBiMap' User Integer
_Admin = Toml.'prism' Admin $ \\__case__
    Admin i -> Right i
    other   -> Toml.'wrongConstructor' \"Admin\" other

_Client :: 'TomlBiMap' User Text
_Client = Toml.'prism' Client $ \\__case__
    Client n -> Right n
    other    -> Toml.'wrongConstructor' \"Client\" other
@
-}
prism :: (field -> object) -> (object -> Either error field) -> BiMap error object field
prism review preview = BiMap preview (Right . review)

----------------------------------------------------------------------------
-- BiMap error types
----------------------------------------------------------------------------

-- | 'BiMap' specialized to TOML error.
type TomlBiMap = BiMap TomlBiMapError

-- | Type of errors for TOML 'BiMap'.
data TomlBiMapError
    = WrongConstructor -- ^ Error for cases with wrong constructors. For
                       -- example, you're trying to convert 'Left' but
                       -- bidirectional converter expects 'Right'.
        Text           -- ^ Expected constructor name
        Text           -- ^ Actual value
    | WrongValue       -- ^ Error for cases with wrong values
        MatchError     -- ^ Information about failed matching
    | ArbitraryError   -- ^ Arbitrary textual error
        Text           -- ^ Error message
    deriving (Eq, Show, Generic, NFData)

-- | Converts 'TomlBiMapError' into pretty human-readable text.
prettyBiMapError :: TomlBiMapError -> Text
prettyBiMapError = \case
    WrongConstructor expected actual -> T.unlines
        [ "Invalid constructor"
        , "  * Expected: " <> expected
        , "  * Actual:   " <> actual
        ]
    WrongValue (MatchError expected actual) -> T.unlines
        [ "Invalid constructor"
        , "  * Expected: " <> tShow expected
        , "  * Actual:   " <> tShow actual
        ]
    ArbitraryError text  -> text

-- | Helper to construct WrongConstuctor error.
wrongConstructor
    :: Show a
    => Text  -- ^ Name of the expected constructor
    -> a     -- ^ Actual value
    -> Either TomlBiMapError b
wrongConstructor constructor x = Left $ WrongConstructor constructor (tShow x)

----------------------------------------------------------------------------
-- General purpose bimaps
----------------------------------------------------------------------------

-- | Bimap for 'Either' and its left type
_Left :: (Show l, Show r) => TomlBiMap (Either l r) l
_Left = prism Left $ \case
    Left l -> Right l
    x -> wrongConstructor "Left" x

-- | Bimap for 'Either' and its right type
_Right :: (Show l, Show r) => TomlBiMap (Either l r) r
_Right = prism Right $ \case
    Right r -> Right r
    x -> wrongConstructor "Right" x

-- | Bimap for 'Maybe'
_Just :: Show r => TomlBiMap (Maybe r) r
_Just = prism Just $ \case
    Just r -> Right r
    x -> wrongConstructor "Just" x

----------------------------------------------------------------------------
--  BiMaps for value
----------------------------------------------------------------------------

-- | Creates prism for 'AnyValue'.
mkAnyValueBiMap
    :: forall a (tag :: TValue) . (forall (t :: TValue) . Value t -> Either MatchError a)
    -> (a -> Value tag)
    -> TomlBiMap a AnyValue
mkAnyValueBiMap matchValue toValue = BiMap
    { forward = Right . toAnyValue
    , backward = fromAnyValue
    }
  where
    toAnyValue :: a -> AnyValue
    toAnyValue = AnyValue . toValue

    fromAnyValue :: AnyValue -> Either TomlBiMapError a
    fromAnyValue (AnyValue value) = first WrongValue $ matchValue value

-- | Creates bimap for 'Data.Text.Text' to 'AnyValue' with custom functions
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

{- | 'Prelude.Bool' bimap for 'AnyValue'. Usually used as
'Toml.Bi.Combinators.bool' combinator.
-}
_Bool :: TomlBiMap Bool AnyValue
_Bool = mkAnyValueBiMap matchBool Bool

{- | 'Prelude.Integer' bimap for 'AnyValue'. Usually used as
'Toml.Bi.Combinators.integer' combinator.
-}
_Integer :: TomlBiMap Integer AnyValue
_Integer = mkAnyValueBiMap matchInteger Integer

{- | 'Prelude.Double' bimap for 'AnyValue'. Usually used as
'Toml.Bi.Combinators.double' combinator.
-}
_Double :: TomlBiMap Double AnyValue
_Double = mkAnyValueBiMap matchDouble Double

{- | 'Data.Text.Text' bimap for 'AnyValue'. Usually used as
'Toml.Bi.Combinators.text' combinator.
-}
_Text :: TomlBiMap Text AnyValue
_Text = mkAnyValueBiMap matchText Text

{- | 'Data.Time.ZonedTime' bimap for 'AnyValue'. Usually used as
'Toml.Bi.Combinators.zonedTime' combinator.
-}
_ZonedTime :: TomlBiMap ZonedTime AnyValue
_ZonedTime = mkAnyValueBiMap matchZoned Zoned

{- | 'Data.Time.LocalTime' bimap for 'AnyValue'. Usually used as
'Toml.Bi.Combinators.localTime' combinator.
-}
_LocalTime :: TomlBiMap LocalTime AnyValue
_LocalTime = mkAnyValueBiMap matchLocal Local

{- | 'Data.Time.Day' bimap for 'AnyValue'. Usually used as
'Toml.Bi.Combinators.day' combinator.
-}
_Day :: TomlBiMap Day AnyValue
_Day = mkAnyValueBiMap matchDay Day

{- | 'Data.Time.TimeOfDay' bimap for 'AnyValue'. Usually used as
'Toml.Bi.Combinators.timeOfDay' combinator.
-}
_TimeOfDay :: TomlBiMap TimeOfDay AnyValue
_TimeOfDay = mkAnyValueBiMap matchHours Hours

-- | Helper bimap for 'String' and 'Data.Text.Text'.
_StringText :: BiMap e String Text
_StringText = iso T.pack T.unpack

{- | 'String' bimap for 'AnyValue'. Usually used as
'Toml.Bi.Combinators.string' combinator.
-}
_String :: TomlBiMap String AnyValue
_String = _StringText >>> _Text

-- | Helper bimap for 'String' and types with 'Read' and 'Show' instances.
_ReadString :: (Show a, Read a) => TomlBiMap a String
_ReadString = BiMap (Right . show) (first (ArbitraryError . T.pack) . readEither)

-- | Bimap for 'AnyValue' and values with a 'Read' and 'Show' instance.
-- Usually used as 'Toml.Bi.Combinators.read' combinator.
_Read :: (Show a, Read a) => TomlBiMap a AnyValue
_Read = _ReadString >>> _String

-- | Helper bimap for 'Natural' and 'Prelude.Integer'.
_NaturalInteger :: TomlBiMap Natural Integer
_NaturalInteger = BiMap (Right . toInteger) eitherInteger
  where
    eitherInteger :: Integer -> Either TomlBiMapError Natural
    eitherInteger n
      | n < 0     = Left $ ArbitraryError $ "Value is below zero, but expected Natural: " <> tShow n
      | otherwise = Right (fromIntegral n)

{- | 'String' bimap for 'AnyValue'. Usually used as
'Toml.Bi.Combinators.natural' combinator.
-}
_Natural :: TomlBiMap Natural AnyValue
_Natural = _NaturalInteger >>> _Integer

-- | Helper bimap for 'Prelude.Integer' and integral, bounded values.
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

{- | 'Word' bimap for 'AnyValue'. Usually used as
'Toml.Bi.Combinators.word' combinator.
-}
_Word :: TomlBiMap Word AnyValue
_Word = _BoundedInteger >>> _Integer

{- | 'Int' bimap for 'AnyValue'. Usually used as
'Toml.Bi.Combinators.int' combinator.
-}
_Int :: TomlBiMap Int AnyValue
_Int = _BoundedInteger >>> _Integer

{- | 'Float' bimap for 'AnyValue'. Usually used as
'Toml.Bi.Combinators.float' combinator.
-}
_Float :: TomlBiMap Float AnyValue
_Float = iso realToFrac realToFrac >>> _Double

-- | Helper bimap for 'Data.Text.Text' and strict 'ByteString'
_ByteStringText :: TomlBiMap ByteString Text
_ByteStringText = prism T.encodeUtf8 eitherText
  where
    eitherText :: ByteString -> Either TomlBiMapError Text
    eitherText = either (\err -> Left $ ArbitraryError $ tShow err) Right . T.decodeUtf8'

-- | UTF8 encoded 'ByteString' bimap for 'AnyValue'.
-- Usually used as 'Toml.Bi.Combinators.byteString' combinator.
_ByteString :: TomlBiMap ByteString AnyValue
_ByteString = _ByteStringText >>> _Text

-- | Helper bimap for 'Data.Text.Text' and lazy 'BL.ByteString'.
_LByteStringText :: TomlBiMap BL.ByteString Text
_LByteStringText = prism (TL.encodeUtf8 . TL.fromStrict) eitherText
  where
    eitherText :: BL.ByteString -> Either TomlBiMapError Text
    eitherText = bimap (ArbitraryError . tShow) TL.toStrict . TL.decodeUtf8'

-- | UTF8 encoded lazy 'BL.ByteString' bimap for 'AnyValue'.
-- Usually used as 'Toml.Bi.Combinators.lazyByteString' combinator.
_LByteString :: TomlBiMap BL.ByteString AnyValue
_LByteString = _LByteStringText >>> _Text

-- | Takes a bimap of a value and returns a bimap between a list of values and 'AnyValue'
-- as an array. Usually used as 'Toml.Bi.Combinators.arrayOf' combinator.
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


-- | Takes a bimap of a value and returns a bimap between a non-empty list of values and 'AnyValue'
-- as an array. Usually used as 'Toml.Bi.Combinators.nonEmpty' combinator.
_NonEmpty :: TomlBiMap a AnyValue -> TomlBiMap (NE.NonEmpty a) AnyValue
_NonEmpty bi = _NonEmptyArray >>> _Array bi

_NonEmptyArray :: TomlBiMap (NE.NonEmpty a) [a]
_NonEmptyArray = BiMap
    { forward  = Right . NE.toList
    , backward = maybe (Left $ ArbitraryError "Empty array list, but expected NonEmpty") Right . NE.nonEmpty
    }

-- | Takes a bimap of a value and returns a bimap between a set of values and 'AnyValue'
-- as an array. Usually used as 'Toml.Bi.Combinators.arraySetOf' combinator.
_Set :: (Ord a) => TomlBiMap a AnyValue -> TomlBiMap (S.Set a) AnyValue
_Set bi = iso S.toList S.fromList >>> _Array bi

-- | Takes a bimap of a value and returns a bimap between a hash set of values and 'AnyValue'
-- as an array. Usually used as 'Toml.Bi.Combinators.arrayHashSetOf' combinator.
_HashSet :: (Eq a, Hashable a) => TomlBiMap a AnyValue -> TomlBiMap (HS.HashSet a) AnyValue
_HashSet bi = iso HS.toList HS.fromList >>> _Array bi

{- | 'IS.IntSet' bimap for 'AnyValue'. Usually used as
'Toml.Bi.Combinators.arrayIntSetOf' combinator.
-}
_IntSet :: TomlBiMap IS.IntSet AnyValue
_IntSet = iso IS.toList IS.fromList >>> _Array _Int

tShow :: Show a => a -> Text
tShow = T.pack . show
