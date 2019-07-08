{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{- | This module contains implementation of the 'Generic' TOML codec. If your
data types are big and nested, and you want to have codecs for them without writing a lot of
boilerplate code, you can find this module helpful. Below you can find the detailed
explanation on how the 'Generic' codecs work.

Consider the following Haskell data types:

@
__data__ User = User
    { age     :: Int
    , address :: Address
    , socials :: [Social]
    } __deriving__ ('Generic')

__data__ Address = Address
    { street :: Text
    , house  :: Int
    } __deriving__ ('Generic')

__data__ Social = Social
    { name :: Text
    , link :: Text
    } __deriving__ ('Generic')
@

Value of the @User@ type represents the following TOML:

@
age = 27

[address]
  street = "Miami Beach"
  house  = 42

[[socials]]
  name = \"Twitter\"
  link = "https://twitter.com/foo"

[[socials]]
  name = \"GitHub\"
  link = "https://github.com/bar"
@

Normally you would write 'TomlCodec' for this data type like this:

@
userCodec :: 'TomlCodec' User
userCodec = User
    \<$\> Toml.int "age" .= age
    \<*\> Toml.table addressCodec "address" .= address
    \<*\> Toml.list  socialCodec  "socials" .= socials

addressCodec :: 'TomlCodec' Address
addressCodec = Address
    \<$\> Toml.text "street" .= street
    \<*\> Toml.int  "house"  .= house

socialCodec :: 'TomlCodec' Social
socialCodec = Social
    \<$\> Toml.text "name" .= name
    \<*\> Toml.text "link" .= link
@

However, if you derive 'Generic' instance for your data types (as we do in the
example), you can write your codecs in a simpler way.

@
userCodec :: 'TomlCodec' User
userCodec = 'genericCodec'

__instance__ 'HasCodec' Address __where__
    hasCodec = Toml.table 'genericCodec'

__instance__ 'HasItemCodec' Social __where__
    hasItemCodec = Right 'genericCodec'
@

Several notes about the interface:

1. Your top-level data types are always implemented as 'genericCodec' (or other
generic codecs).
2. If you have a custom data type as a field of another type, you need to implement
the instance of the 'HasCodec' typeclass.
3. If the data type appears as an element of a list, you need to implement the instance
of the 'HasItemCodec' typeclass.

@since 1.1.1.0
-}

module Toml.Generic
       ( genericCodec
       , genericCodecWithOptions
       , stripTypeNameCodec

         -- * Options
       , TomlOptions (..)
       , GenericOptions (..)
       , stripTypeNameOptions
       , stripTypeNamePrefix

         -- * Core generic typeclass
       , HasCodec (..)
       , HasItemCodec (..)
       , GenericCodec (..)
       ) where

import Data.Char (isLower, toLower)
import Data.IntSet (IntSet)
import Data.Kind (Type)
import Data.List (stripPrefix)
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Time (Day, LocalTime, TimeOfDay, ZonedTime)
import Data.Typeable (Typeable, typeRep)
import Data.Word (Word)
import GHC.Generics ((:*:) (..), (:+:), C1, D1, Generic (..), K1 (..), M1 (..), Rec0, S1,
                     Selector (..))
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Numeric.Natural (Natural)

import Toml.Bi (TomlBiMap, TomlCodec, (.=))
import Toml.PrefixTree (Key)
import Toml.Type (AnyValue)

import qualified Data.Text.Lazy as L
import qualified Toml.Bi as Toml


{- | Generic codec for arbitrary data types. Uses field names as keys.
-}
genericCodec :: (Generic a, GenericCodec (Rep a)) => TomlCodec a
genericCodec = Toml.dimap from to $ genericTomlCodec (GenericOptions id)
{-# INLINE genericCodec #-}

{- | Generic codec with options for arbitrary data types.
-}
genericCodecWithOptions
    :: forall a
     . (Generic a, GenericCodec (Rep a), Typeable a)
    => TomlOptions a
    -> TomlCodec a
genericCodecWithOptions = Toml.dimap from to . genericTomlCodec . toGenericOptions @a
{-# INLINE genericCodecWithOptions #-}

{- | Generic codec that uses 'stripTypeNameOptions'.
-}
stripTypeNameCodec
    :: forall a
     . (Generic a, GenericCodec (Rep a), Typeable a)
    => TomlCodec a
stripTypeNameCodec = genericCodecWithOptions $ stripTypeNameOptions @a
{-# INLINE stripTypeNameCodec #-}

----------------------------------------------------------------------------
-- Generic typeclasses
----------------------------------------------------------------------------

{- | Options to configure various parameters of generic encoding. Specifically:

*  __'tomlOptionsFieldModifier'__: how to translate field names to TOML keys?
-}
data TomlOptions a = TomlOptions
    { tomlOptionsFieldModifier :: Typeable a => Proxy a -> String -> String
    }

{- | Same as 'TomlOptions' but with all data type information erased. This data
type is used internally. Define your options using 'TomlOptions' data type.
-}
newtype GenericOptions = GenericOptions
    { genericOptionsFieldModifier :: String -> String
    }

toGenericOptions :: forall a . Typeable a => TomlOptions a -> GenericOptions
toGenericOptions TomlOptions{..} = GenericOptions
    { genericOptionsFieldModifier = tomlOptionsFieldModifier (Proxy @a)
    }

-- | Options that use 'stripTypeNamePrefix' as 'tomlOptionsFieldModifier'.
stripTypeNameOptions :: Typeable a => TomlOptions a
stripTypeNameOptions = TomlOptions
    { tomlOptionsFieldModifier = stripTypeNamePrefix
    }

{- | Strips name of the type name from field name prefix.

>>> data UserData = UserData { userDataId :: Int, userDataShortInfo :: Text }
>>> stripTypeNamePrefix (Proxy @UserData) "userDataId"
"id"
>>> stripTypeNamePrefix (Proxy @UserData) "userDataShortInfo"
"shortInfo"
>>> stripTypeNamePrefix (Proxy @UserData) "udStats"
"stats"
>>> stripTypeNamePrefix (Proxy @UserData) "fooBar"
"bar"
>>> stripTypeNamePrefix (Proxy @UserData) "name"
"name"
-}
stripTypeNamePrefix :: forall a . Typeable a => Proxy a -> String -> String
stripTypeNamePrefix _ fieldName =
    case stripPrefix (headToLower $ typeName @a) fieldName of
        Just rest -> leaveIfEmpty rest
        Nothing   -> leaveIfEmpty (dropWhile isLower fieldName)
  where
    headToLower :: String -> String
    headToLower = \case
        []   -> error "Cannot use 'headToLower' on empty Text"
        x:xs -> toLower x : xs

    -- if all lower case then leave field as it is
    leaveIfEmpty :: String -> String
    leaveIfEmpty rest = if null rest then fieldName else headToLower rest

typeName :: forall a . Typeable a => String
typeName = show $ typeRep (Proxy @a)

----------------------------------------------------------------------------
-- Generic typeclasses
----------------------------------------------------------------------------

class GenericCodec (f :: k -> Type) where
    genericTomlCodec :: GenericOptions -> TomlCodec (f p)

instance GenericCodec f => GenericCodec (D1 d f) where
    genericTomlCodec = Toml.dimap unM1 M1 . genericTomlCodec
    {-# INLINE genericTomlCodec #-}

type GenericSumTomlNotSupported =
    'Text "Generic TOML deriving for arbitrary sum types is not supported currently."

instance (TypeError GenericSumTomlNotSupported) => GenericCodec (f :+: g) where
    genericTomlCodec = error "Not supported"

instance GenericCodec f => GenericCodec (C1 c f) where
    genericTomlCodec = Toml.dimap unM1 M1 . genericTomlCodec
    {-# INLINE genericTomlCodec #-}

instance (GenericCodec f, GenericCodec g) => GenericCodec (f :*: g) where
    genericTomlCodec options = (:*:)
        <$> genericTomlCodec options .= fstG
        <*> genericTomlCodec options .= sndG
      where
        fstG :: (f :*: g) p -> f p
        fstG (f :*: _) = f

        sndG :: (f :*: g) p -> g p
        sndG (_ :*: g) = g
    {-# INLINE genericTomlCodec #-}

instance (Selector s, HasCodec a) => GenericCodec (S1 s (Rec0 a)) where
    genericTomlCodec GenericOptions{..} = genericWrap $ hasCodec @a fieldName
      where
        genericWrap :: TomlCodec a -> TomlCodec (S1 s (Rec0 a) p)
        genericWrap = Toml.dimap (unK1 . unM1) (M1 . K1)

        fieldName :: Key
        fieldName =
            fromString
            $ genericOptionsFieldModifier
            $ selName (error "S1" :: S1 s Proxy ())
    {-# INLINE genericTomlCodec #-}

----------------------------------------------------------------------------
-- Helper typeclasses
----------------------------------------------------------------------------

{- | This typeclass tells how the data type should be coded as an item of a
list. Lists in TOML can have two types: __primitive__ and __table of arrays__.

* If 'hasItemCodec' returns 'Left': __primitive__ arrays codec is used.
* If 'hasItemCodec' returns 'Right:' __table of arrays__ codec is used.
-}
class HasItemCodec a where
    hasItemCodec :: Either (TomlBiMap a AnyValue) (TomlCodec a)

instance HasItemCodec Bool      where hasItemCodec = Left Toml._Bool
instance HasItemCodec Int       where hasItemCodec = Left Toml._Int
instance HasItemCodec Word      where hasItemCodec = Left Toml._Word
instance HasItemCodec Integer   where hasItemCodec = Left Toml._Integer
instance HasItemCodec Natural   where hasItemCodec = Left Toml._Natural
instance HasItemCodec Double    where hasItemCodec = Left Toml._Double
instance HasItemCodec Float     where hasItemCodec = Left Toml._Float
instance HasItemCodec Text      where hasItemCodec = Left Toml._Text
instance HasItemCodec L.Text    where hasItemCodec = Left Toml._LText
instance HasItemCodec ZonedTime where hasItemCodec = Left Toml._ZonedTime
instance HasItemCodec LocalTime where hasItemCodec = Left Toml._LocalTime
instance HasItemCodec Day       where hasItemCodec = Left Toml._Day
instance HasItemCodec TimeOfDay where hasItemCodec = Left Toml._TimeOfDay
instance HasItemCodec IntSet    where hasItemCodec = Left Toml._IntSet

{- | If data type @a@ is not primitive then this instance returns codec for list
under key equal to @a@ type name.
-}
instance (HasItemCodec a, Typeable a) => HasItemCodec [a] where
    hasItemCodec = case hasItemCodec @a of
        Left prim   -> Left $ Toml._Array prim
        Right codec -> Right $ Toml.list codec (fromString $ typeName @a)

{- | Helper typeclass for generic deriving. This instance tells how the data
type should be coded if it's a field of another data type.

__NOTE:__ If you implement TOML codecs for your data types manually, prefer more
explicit @Toml.int@ or @Toml.text@ instead of implicit @Toml.hasCodec@.
Implement instances of this typeclass only when using 'genericCodec' and when
your custom data types are not covered here.
-}
class HasCodec a where
    hasCodec :: Key -> TomlCodec a

instance HasCodec Bool      where hasCodec = Toml.bool
instance HasCodec Int       where hasCodec = Toml.int
instance HasCodec Word      where hasCodec = Toml.word
instance HasCodec Integer   where hasCodec = Toml.integer
instance HasCodec Natural   where hasCodec = Toml.natural
instance HasCodec Double    where hasCodec = Toml.double
instance HasCodec Float     where hasCodec = Toml.float
instance HasCodec Text      where hasCodec = Toml.text
instance HasCodec L.Text    where hasCodec = Toml.lazyText
instance HasCodec ZonedTime where hasCodec = Toml.zonedTime
instance HasCodec LocalTime where hasCodec = Toml.localTime
instance HasCodec Day       where hasCodec = Toml.day
instance HasCodec TimeOfDay where hasCodec = Toml.timeOfDay
instance HasCodec IntSet    where hasCodec = Toml.arrayIntSet

instance HasCodec a => HasCodec (Maybe a) where
    hasCodec = Toml.dioptional . hasCodec @a

instance HasItemCodec a => HasCodec [a] where
    hasCodec = case hasItemCodec @a of
        Left prim   -> Toml.arrayOf prim
        Right codec -> Toml.list codec

instance HasItemCodec a => HasCodec (NonEmpty a) where
    hasCodec = case hasItemCodec @a of
        Left prim   -> Toml.arrayNonEmptyOf prim
        Right codec -> Toml.nonEmpty codec

{-
TODO: uncomment when higher-kinded roles will be implemented
* https://github.com/ghc-proposals/ghc-proposals/pull/233

{- | @newtype@ for generic deriving of 'HasCodec' typeclass for custom data
types that should we wrapped into separate table. Use it only for data types
that are fields of another data types.

@
data Person = Person
    { personName    :: Text
    , personAddress :: Address
    } deriving (Generic)

data Address = Address
    { addressStreet :: Text
    , addressHouse  :: Int
    } deriving (Generic)
      deriving HasCodec via TomlTable Address

personCodec :: TomlCodec Person
personCodec = genericCodec
@

@personCodec@ corresponds to the TOML of the following structure:

@
name = "foo"
[address]
    street = \"Bar\"
    house = 42
@
-}
newtype TomlTable a = TomlTable
    { unTomlTable :: a
    }

instance (Generic a, GenericCodec (Rep a)) => HasCodec (TomlTable a) where
    hasCodec :: Key -> TomlCodec (TomlTable a)
    hasCodec = Toml.diwrap . Toml.table (genericCodec @a)

instance (Generic a, GenericCodec (Rep a)) => HasItemCodec (TomlTable a) where
    hasItemCodec = Right $ Toml.diwrap $ genericCodec @a
-}
