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

-- | This module contains implementation of the 'Generic' TOML codec.

module Toml.Generic
       ( genericCodec
       , genericCodecWithOptions
       , stripTypeNameCodec

         -- * Options
       , TomlOptions (..)
       , stripTypeNameOptions

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


{- | Generic codec for arbitrary data types.
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

{- | Same as 'TomlOptions' but with all data type information erased.
-}
newtype GenericOptions = GenericOptions
    { genericOptionsFieldModifier :: String -> String
    }

toGenericOptions :: forall a . Typeable a => TomlOptions a -> GenericOptions
toGenericOptions TomlOptions{..} = GenericOptions
    { genericOptionsFieldModifier = tomlOptionsFieldModifier (Proxy @a)
    }

{- | Options to strip the prefix.
-}
stripTypeNameOptions :: Typeable a => TomlOptions a
stripTypeNameOptions = TomlOptions
    { tomlOptionsFieldModifier = stripTypeNamePrefix
    }

{- | Strips name of the type name from field name prefix.
-}
stripTypeNamePrefix :: forall a . Typeable a => Proxy a -> String -> String
stripTypeNamePrefix proxy fieldName =
    case stripPrefix (headToLower typeName) fieldName of
        Just rest -> leaveIfEmpty rest
        Nothing   -> leaveIfEmpty (dropWhile isLower fieldName)
  where
    typeName :: String
    typeName = show $ typeRep proxy

    headToLower :: String -> String
    headToLower = \case
        []   -> error "Cannot use 'headToLower' on empty Text"
        x:xs -> toLower x : xs

    -- if all lower case then leave field as it is
    leaveIfEmpty :: String -> String
    leaveIfEmpty rest = if null rest then fieldName else headToLower rest

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

{- | This typeclass tells how the data type should be coded as an item of the
list.
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

instance HasItemCodec a => HasItemCodec [a] where
    hasItemCodec = case hasItemCodec @a of
        Left prim   -> Left $ Toml._Array prim
        Right codec -> Right $ Toml.dilist codec

{- | Helper typeclass for generic deriving. You can use this typeclass for
writing your custom codecs manually but this is less explicit and generally
not encouraged. Implement instances of this typeclass only if some data types
are not covered here.
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

{- TODO: uncomment when higher-kinded roles will be implemented

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
