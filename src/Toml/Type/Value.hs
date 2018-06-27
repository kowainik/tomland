{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Toml.Type.Value
       ( -- * Type of value
         TValue (..)
       , showType

         -- * Value
       , Value (..)
       , DateTime (..)
       , eqValueList
       , valueType

         -- * Type checking
       , TypeMismatchError (..)
       , sameValue
       ) where

import Data.String (IsString (..))
import Data.Text (Text)
import Data.Time (Day, LocalTime, TimeOfDay, ZonedTime, zonedTimeToUTC)
import Data.Type.Equality ((:~:) (..))

-- | Needed for GADT parameterization
data TValue = TBool | TInteger | TDouble | TText | TDate | TArray
    deriving (Eq, Show)

showType :: TValue -> String
showType = drop 1 . show

-- TODO: examples are copy-pasted from TOML specification. Probably most of them
-- will be moved into parsing module in future.
-- | Value in @key = value@ pair.
data Value (t :: TValue) where
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
    Integer :: Integer -> Value 'TInteger

    {- | Floating point number:

@
flt1 = -3.1415   # fractional
flt2 = 1e6       # exponent
flt3 = 6.626e-34 # both
flt4 = 9_224_617.445_991_228_313
@
    -}
    Double :: Double -> Value 'TDouble

    {- | String value:

@
key = "value"
bare_key = "value"
bare-key = "value"
@
    -}
    Text :: Text -> Value 'TText

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

deriving instance Show (Value t)

instance (t ~ 'TInteger) => Num (Value t) where
    (Integer a) + (Integer b) = Integer $ a + b
    (Integer a) * (Integer b) = Integer $ a * b
    abs (Integer a) = Integer (abs a)
    signum (Integer a) = Integer (signum a)
    fromInteger = Integer
    negate (Integer a) = Integer (negate a)

instance (t ~ 'TText) => IsString (Value t) where
    fromString = Text . fromString @Text

instance Eq (Value t) where
    (Bool b1)    == (Bool b2)    = b1 == b2
    (Integer i1) == (Integer i2) = i1 == i2
    (Double f1)  == (Double f2)  = f1 == f2
    (Text s1)    == (Text s2)    = s1 == s2
    (Date d1)    == (Date d2)    = d1 == d2
    (Array a1)   == (Array a2)   = eqValueList a1 a2

eqValueList :: [Value a] -> [Value b] -> Bool
eqValueList [] [] = True
eqValueList (x:xs) (y:ys) = case sameValue x y of
    Right Refl -> x == y && eqValueList xs ys
    Left _     -> False
eqValueList _ _ = False

-- | Reifies type of 'Value' into 'ValueType'. Unfortunately, there's no way to
-- guarante that 'valueType' will return @t@ for object with type @Value \'t@.
valueType :: Value t -> TValue
valueType (Bool _)    = TBool
valueType (Integer _) = TInteger
valueType (Double _)  = TDouble
valueType (Text _)    = TText
valueType (Date _)    = TDate
valueType (Array _)   = TArray

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
    deriving (Show)

instance Eq DateTime where
    (Zoned a) == (Zoned b) = zonedTimeToUTC a == zonedTimeToUTC b
    (Local a) == (Local b) = a == b
    (Day a)   == (Day b)   = a == b
    (Hours a) == (Hours b) = a == b
    _         == _         = False

----------------------------------------------------------------------------
-- Typechecking values
----------------------------------------------------------------------------

-- | Data type that holds expected vs. actual type.
data TypeMismatchError = TypeMismatchError
  { typeExpected :: TValue
  , typeActual   :: TValue
  } deriving (Eq)

instance Show TypeMismatchError where
    show TypeMismatchError{..} = "Expected type '" ++ showType typeExpected
                              ++ "' but actual type: '" ++ showType typeActual ++ "'"

sameValue :: Value a -> Value b -> Either TypeMismatchError (a :~: b)
sameValue Bool{}    Bool{}    = Right Refl
sameValue Integer{} Integer{} = Right Refl
sameValue Double{}  Double{}  = Right Refl
sameValue Text{}    Text{}    = Right Refl
sameValue Date{}    Date{}    = Right Refl
sameValue Array{}   Array{}   = Right Refl
sameValue l         r         = Left $ TypeMismatchError
                                         { typeExpected = valueType l
                                         , typeActual   = valueType r
                                         }
