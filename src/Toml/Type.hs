{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE TypeOperators             #-}

{- | Contains specification of TOML via Haskell ADT. -}

module Toml.Type
       ( TOML (..)
       , UValue (..)
       , Value (..)
       , AnyValue (..)
       , DateTime (..)
       , typeCheck
       ) where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Time (Day, LocalTime, TimeOfDay, ZonedTime)
import Data.Type.Equality ((:~:) (..))

import Toml.PrefixTree (Key (..), PrefixMap)

-- TODO: describe how some TOML document will look like with this type
{- | Represents TOML configuration value. -}
data TOML = TOML
    { tomlPairs  :: HashMap Key AnyValue
    , tomlTables :: PrefixMap TOML
    -- tomlTableArrays :: HashMap Key (NonEmpty TOML)
    }

-- Needed for GADT parameterization
data ValueType = TBool | TInt | TFloat | TString | TDate | TArray

-- TODO: examples are copy-pasted from TOML specification. Probably most of them
-- will be moved into parsing module in future.
-- | Value in @key = value@ pair.
data Value (t :: ValueType) where
    {- | Boolean value:

@
bool1 = true
bool2 = false
@
    -}
    Bool :: Bool -> Value 'TBool

    {- | Integer value:

@
int1 = +99
int2 = 42
int3 = 0
int4 = -17
int5 = 5_349_221
hex1 = 0xDEADBEEF
oct2 = 0o755 # useful for Unix file permissions
bin1 = 0b11010110
@
    -}
    Int :: Integer -> Value 'TInt

    {- | Floating point number:

@
flt1 = -3.1415   # fractional
flt2 = 1e6       # exponent
flt3 = 6.626e-34 # both
flt4 = 9_224_617.445_991_228_313
@
    -}
    Float :: Double -> Value 'TFloat

    {- | String value:

@
key = "value"
bare_key = "value"
bare-key = "value"
@
    -}
    String :: Text -> Value 'TString

    -- | Date-time. See documentation for 'DateTime' type.
    Date :: DateTime -> Value 'TDate

    {- | Array of values. According to TOML specification all values in array
      should have the same type. This is guaranteed statically with this type.

@
arr1 = [ 1, 2, 3 ]
arr2 = [ "red", "yellow", "green" ]
arr3 = [ [ 1, 2 ], [3, 4, 5] ]
arr4 = [ "all", 'strings', """are the same""", '''type''']
arr5 = [ [ 1, 2 ], ["a", "b", "c"] ]

arr6 = [ 1, 2.0 ] # INVALID
@
    -}
    Array  :: [Value t] -> Value 'TArray

-- TODO: move into Toml.Type.Internal module then?.. But it uses 'DateTime' which is not internal...
-- | Untyped value of 'TOML'. You shouldn't use this type in your code. Use
-- 'Value' instead.
data UValue
    = UBool !Bool
    | UInt !Integer
    | UFloat !Double
    | UString !Text
    | UDate !DateTime
    | UArray ![UValue]

-- | Existential wrapper for 'Value'.
data AnyValue = forall (t :: ValueType) . AnyValue (Value t)

data DateTime
      {- | Offset date-time:

@
odt1 = 1979-05-27T07:32:00Z
odt2 = 1979-05-27T00:32:00-07:00
@
      -}
    = Zoned !ZonedTime

      {- | Local date-time (without offset):

@
ldt1 = 1979-05-27T07:32:00
ldt2 = 1979-05-27T00:32:00.999999
@
      -}
    | Local !LocalTime

      {- | Local date (only day):

@
ld1 = 1979-05-27
@
      -}
    | Day !Day

      {- | Local time (time of the day):

@
lt1 = 07:32:00
lt2 = 00:32:00.999999

@
      -}
    | Hours !TimeOfDay

-- | Ensures that 'UValue's represents type-safe version of @toml@.
typeCheck :: UValue -> Maybe AnyValue
typeCheck (UBool b)   = justAny $ Bool b
typeCheck (UInt n)    = justAny $ Int n
typeCheck (UFloat f)  = justAny $ Float f
typeCheck (UString s) = justAny $ String s
typeCheck (UDate d)   = justAny $ Date d
typeCheck (UArray a)  = case a of
    []     -> justAny $ Array []
    (x:xs) -> do
        AnyValue v <- typeCheck x
        AnyValue . Array <$> checkElem v xs
  where
    checkElem :: Value t -> [UValue] -> Maybe [Value t]
    checkElem v []     = Just [v]
    checkElem v (x:xs) = do
        AnyValue vx <- typeCheck x
        Refl <- sameValue v vx
        (v :) <$> checkElem vx xs

justAny :: Value t -> Maybe AnyValue
justAny = Just . AnyValue

sameValue :: Value a -> Value b -> Maybe (a :~: b)
sameValue Bool{}   Bool{}   = Just Refl
sameValue Int{}    Int{}    = Just Refl
sameValue Float{}  Float{}  = Just Refl
sameValue String{} String{} = Just Refl
sameValue Date{}   Date{}   = Just Refl
sameValue Array{}  Array{}  = Just Refl
sameValue _        _        = Nothing
