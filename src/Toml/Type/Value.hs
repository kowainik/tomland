{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

{- |
Module                  : Toml.Type.Value
Copyright               : (c) 2018-2021 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

GADT value for TOML.

@since 0.0.0
-}

module Toml.Type.Value
       ( -- * Type of value
         TValue (..)
       , showType

         -- * Value
       , Value (..)
       , eqValueList
       , valueType

         -- * Type checking
       , TypeMismatchError (..)
       , sameValue
       ) where

import Control.DeepSeq (NFData (..), rnf)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Time (Day, LocalTime, TimeOfDay, ZonedTime, zonedTimeToUTC)
import Data.Type.Equality ((:~:) (..))
import GHC.Generics (Generic)


{- | Needed for GADT parameterization

@since 0.0.0
-}
data TValue
    = TBool
    | TInteger
    | TDouble
    | TText
    | TZoned
    | TLocal
    | TDay
    | THours
    | TArray
    deriving stock (Eq, Show, Read, Generic)
    deriving anyclass (NFData)

{- | Convert 'TValue' constructors to 'String' without @T@ prefix.

@since 0.0.0
-}
showType :: TValue -> String
showType = drop 1 . show

{- | Value in @key = value@ pair.

@since 0.0.0
-}
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
hex1 = 0xDEADBEEF  # hexadecimal
oct2 = 0o755  # octal, useful for Unix file permissions
bin1 = 0b11010110  # binary
@
    -}
    Integer :: Integer -> Value 'TInteger

    {- | Floating point number:

@
# fractional
flt1 = +1.0
flt2 = 3.1415
flt3 = -0.01

# exponent
flt4 = 5e+22
flt5 = 1e6
flt6 = -2E-2

# both
flt7 = 6.626e-34

# infinity
sf1 = inf  # positive infinity
sf2 = +inf # positive infinity
sf3 = -inf # negative infinity

# not a number
sf4 = nan  # actual sNaN/qNaN encoding is implementation specific
sf5 = +nan # same as \`nan\`
sf6 = -nan # same as \`nan\`
@
    -}
    Double :: Double -> Value 'TDouble

    {- | String value:

@
# basic string
name = \"Orange\"
physical.color = "orange"
physical.shape = "round"

# multiline basic string
str1 = """
Roses are red
Violets are blue"""

# literal string: What you see is what you get.
winpath  = 'C:\Users\nodejs\templates'
winpath2 = '\\ServerX\admin$\system32\'
quoted   = 'Tom \"Dubs\" Preston-Werner'
regex    = '<\i\c*\s*>'
@
    -}
    Text :: Text -> Value 'TText

    {- | Offset date-time:

@
odt1 = 1979-05-27T07:32:00Z
odt2 = 1979-05-27T00:32:00-07:00
odt3 = 1979-05-27T00:32:00.999999-07:00
@
    -}
    Zoned :: ZonedTime -> Value 'TZoned

    {- | Local date-time (without offset):

@
ldt1 = 1979-05-27T07:32:00
ldt2 = 1979-05-27T00:32:00.999999
@
    -}
    Local :: LocalTime -> Value 'TLocal

    {- | Local date (only day):

@
ld1 = 1979-05-27
@
    -}
    Day :: Day -> Value 'TDay

    {- | Local time (time of the day):

@
lt1 = 07:32:00
lt2 = 00:32:00.999999

@
    -}
    Hours :: TimeOfDay -> Value 'THours

    {- | Array of values. According to TOML specification all values in array
      should have the same type. This is guaranteed statically with this type.

@
arr1 = [ 1, 2, 3 ]
arr2 = [ "red", "yellow", "green" ]
arr3 = [ [ 1, 2 ], [3, 4, 5] ]
arr4 = [ "all", \'strings\', """are the same""", \'\'\'type\'\'\']
arr5 = [ [ 1, 2 ], ["a", "b", "c"] ]

arr6 = [ 1, 2.0 ] # INVALID
@
    -}
    Array  :: [Value t] -> Value 'TArray

-- | @since 0.0.0
deriving stock instance Show (Value t)

instance NFData (Value t) where
    rnf (Bool n)    = rnf n
    rnf (Integer n) = rnf n
    rnf (Double n)  = rnf n
    rnf (Text n)    = rnf n
    rnf (Zoned n)   = rnf n
    rnf (Local n)   = rnf n
    rnf (Day n)     = rnf n
    rnf (Hours n)   = rnf n
    rnf (Array n)   = rnf n

instance (t ~ 'TInteger) => Num (Value t) where
    (Integer a) + (Integer b) = Integer $ a + b
    (Integer a) * (Integer b) = Integer $ a * b
    abs (Integer a) = Integer (abs a)
    signum (Integer a) = Integer (signum a)
    fromInteger = Integer
    negate (Integer a) = Integer (negate a)

instance (t ~ 'TText) => IsString (Value t) where
    fromString = Text . fromString @Text
    {-# INLINE fromString #-}

instance Eq (Value t) where
    (Bool b1)    == (Bool b2)    = b1 == b2
    (Integer i1) == (Integer i2) = i1 == i2
    (Double f1)  == (Double f2)
        | isNaN f1 && isNaN f2 = True
        | otherwise = f1 == f2
    (Text s1)    == (Text s2)    = s1 == s2
    (Zoned a)    == (Zoned b)    = zonedTimeToUTC a == zonedTimeToUTC b
    (Local a)    == (Local b)    = a == b
    (Day a)      == (Day b)      = a == b
    (Hours a)    == (Hours b)    = a == b
    (Array a1)   == (Array a2)   = eqValueList a1 a2

{- | Compare list of 'Value' of possibly different types.

@since 0.0.0
-}
eqValueList :: [Value a] -> [Value b] -> Bool
eqValueList [] [] = True
eqValueList (x:xs) (y:ys) = case sameValue x y of
    Right Refl -> x == y && eqValueList xs ys
    Left _     -> False
eqValueList _ _ = False

{- | Reifies type of 'Value' into 'TValue'. Unfortunately, there's no
way to guarantee that 'valueType' will return @t@ for object with type
@Value \'t@.

@since 0.0.0
-}
valueType :: Value t -> TValue
valueType (Bool _)    = TBool
valueType (Integer _) = TInteger
valueType (Double _)  = TDouble
valueType (Text _)    = TText
valueType (Zoned _)   = TZoned
valueType (Local _)   = TLocal
valueType (Day _)     = TDay
valueType (Hours _)   = THours
valueType (Array _)   = TArray

----------------------------------------------------------------------------
-- Typechecking values
----------------------------------------------------------------------------

{- | Data type that holds expected vs. actual type.

@since 0.1.0
-}
data TypeMismatchError = TypeMismatchError
  { typeExpected :: !TValue
  , typeActual   :: !TValue
  } deriving stock (Eq)

-- | @since 0.1.0
instance Show TypeMismatchError where
    show TypeMismatchError{..} = "Expected type '" ++ showType typeExpected
                              ++ "' but actual type: '" ++ showType typeActual ++ "'"

{- | Checks whether two values are the same. This function is used for type
checking where first argument is expected type and second argument is actual
type.

@since 0.0.0
-}
sameValue :: Value a -> Value b -> Either TypeMismatchError (a :~: b)
sameValue Bool{}    Bool{}    = Right Refl
sameValue Integer{} Integer{} = Right Refl
sameValue Double{}  Double{}  = Right Refl
sameValue Text{}    Text{}    = Right Refl
sameValue Zoned{}   Zoned{}   = Right Refl
sameValue Local{}   Local{}   = Right Refl
sameValue Day{}     Day{}     = Right Refl
sameValue Hours{}   Hours{}   = Right Refl
sameValue Array{}   Array{}   = Right Refl
sameValue l         r         = Left $ TypeMismatchError
                                         { typeExpected = valueType l
                                         , typeActual   = valueType r
                                         }
