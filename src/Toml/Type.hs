{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeOperators             #-}

{- | Contains specification of TOML via Haskell ADT. -}

module Toml.Type
       ( -- * Main type
         TOML (..)
       , insertKeyVal
       , insertTable

         -- * Values
       , ValueType (..)
       , Value (..)
       , AnyValue (..)
       , UValue (..)
       , DateTime (..)

         -- * Matching
       , liftMatch
       , matchBool
       , matchInteger
       , matchDouble
       , matchText
       , matchDate
       , matchArray

         -- * Typechecking value functions
       , TypeMismatchError (..)
       , reifyAnyValues
       , showType
       , typeCheck
       , valueType
       ) where

import Data.HashMap.Strict (HashMap)
import Data.Semigroup (Semigroup (..))
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Time (Day, LocalTime, TimeOfDay, ZonedTime, zonedTimeToUTC)
import Data.Type.Equality ((:~:) (..))

import Toml.PrefixTree (Key (..), PrefixMap)

import qualified Data.HashMap.Strict as HashMap
import qualified Toml.PrefixTree as Prefix

-- TODO: describe how some TOML document will look like with this type
{- | Represents TOML configuration value. -}
data TOML = TOML
    { tomlPairs  :: HashMap Key AnyValue
    , tomlTables :: PrefixMap TOML
    -- tomlTableArrays :: HashMap Key (NonEmpty TOML)
    } deriving (Show, Eq)

instance Semigroup TOML where
    (TOML pairsA tablesA) <> (TOML pairsB tablesB) = TOML
        (pairsA <> pairsB)
        (HashMap.unionWith (<>) tablesA tablesB)

instance Monoid TOML where
    mappend = (<>)
    mempty = TOML mempty mempty

-- | Inserts given key-value into the 'TOML'.
insertKeyVal :: Key -> Value a -> TOML -> TOML
insertKeyVal k v toml = toml {tomlPairs = HashMap.insert k (AnyValue v) (tomlPairs toml)}

-- | Inserts given table into the 'TOML'.
insertTable :: Key -> TOML -> TOML -> TOML
insertTable k inToml toml = toml
    { tomlTables = Prefix.insert k inToml (tomlTables toml)
    }

-- | Needed for GADT parameterization
data ValueType = TBool | TInt | TFloat | TString | TDate | TArray
    deriving (Eq, Show)

showType :: ValueType -> String
showType = drop 1 . show

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

deriving instance Show (Value t)

instance (t ~ 'TInt) => Num (Value t) where
    (Int a) + (Int b) = Int $ a + b
    (Int a) * (Int b) = Int $ a * b
    abs (Int a) = Int (abs a)
    signum (Int a) = Int (signum a)
    fromInteger = Int
    negate (Int a) = Int (negate a)

instance (t ~ 'TString) => IsString (Value t) where
    fromString = String . fromString @Text

instance Eq (Value t) where
    (Bool b1)   == (Bool b2)   = b1 == b2
    (Int i1)    == (Int i2)    = i1 == i2
    (Float f1)  == (Float f2)  = f1 == f2
    (String s1) == (String s2) = s1 == s2
    (Date d1)   == (Date d2)   = d1 == d2
    (Array a1)  == (Array a2)  = eqValueList a1 a2

eqValueList :: [Value a] -> [Value b] -> Bool
eqValueList [] [] = True
eqValueList (x:xs) (y:ys) = case sameValue x y of
    Right Refl -> x == y && eqValueList xs ys
    Left _     -> False
eqValueList _ _ = False

-- | Reifies type of 'Value' into 'ValueType'. Unfortunately, there's no way to
-- guarante that 'valueType' will return @t@ for object with type @Value \'t@.
valueType :: Value t -> ValueType
valueType (Bool _)   = TBool
valueType (Int _)    = TInt
valueType (Float _)  = TFloat
valueType (String _) = TString
valueType (Date _)   = TDate
valueType (Array _)  = TArray

----------------------------------------------------------------------------
-- Matching functions for values
----------------------------------------------------------------------------

-- | Extract 'Bool' from 'Value'.
matchBool :: Value t -> Maybe Bool
matchBool (Bool b) = Just b
matchBool _        = Nothing

-- | Extract 'Integer' from 'Value'.
matchInteger :: Value t -> Maybe Integer
matchInteger (Int n) = Just n
matchInteger _       = Nothing

-- | Extract 'Double' from 'Value'.
matchDouble :: Value t -> Maybe Double
matchDouble (Float f) = Just f
matchDouble _         = Nothing

-- | Extract 'Text' from 'Value'.
matchText :: Value t -> Maybe Text
matchText (String s) = Just s
matchText _          = Nothing

-- | Extract 'DateTime' from 'Value'.
matchDate :: Value t -> Maybe DateTime
matchDate (Date d) = Just d
matchDate _        = Nothing

-- | Extract list of elements of type @a@ from array.
matchArray :: (AnyValue -> Maybe a) -> Value t -> Maybe [a]
matchArray matchValue (Array a) = mapM (liftMatch matchValue) a
matchArray _            _       = Nothing

liftMatch :: (AnyValue -> Maybe a) -> (Value t -> Maybe a)
liftMatch fromAnyValue = fromAnyValue . AnyValue

----------------------------------------------------------------------------
-- Untyped value
----------------------------------------------------------------------------

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
    deriving (Eq, Show)

-- | Existential wrapper for 'Value'.
data AnyValue = forall (t :: ValueType) . AnyValue (Value t)

instance Show AnyValue where
    show (AnyValue v) = show v

instance Eq AnyValue where
    (AnyValue (Bool b1))   == (AnyValue (Bool b2))   = b1 == b2
    (AnyValue (Int i1))    == (AnyValue (Int i2))    = i1 == i2
    (AnyValue (Float f1))  == (AnyValue (Float f2))  = f1 == f2
    (AnyValue (String s1)) == (AnyValue (String s2)) = s1 == s2
    (AnyValue (Date d1))   == (AnyValue (Date d2))   = d1 == d2
    (AnyValue (Array a1))  == (AnyValue (Array a2))  = eqValueList a1 a2
    _                      == _                      = False


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

-- | Data type that holds expected vs. actual type.
data TypeMismatchError = TypeMismatchError
  { typeExpected :: ValueType
  , typeActual   :: ValueType
  } deriving (Eq)

instance Show TypeMismatchError where
    show TypeMismatchError{..} = "Expected type '" ++ showType typeExpected
                              ++ "' but actual type: '" ++ showType typeActual ++ "'"

-- | Ensures that 'UValue's represents type-safe version of @toml@.
typeCheck :: UValue -> Either TypeMismatchError AnyValue
typeCheck (UBool b)   = rightAny $ Bool b
typeCheck (UInt n)    = rightAny $ Int n
typeCheck (UFloat f)  = rightAny $ Float f
typeCheck (UString s) = rightAny $ String s
typeCheck (UDate d)   = rightAny $ Date d
typeCheck (UArray a)  = case a of
    []   -> rightAny $ Array []
    x:xs -> do
        AnyValue v <- typeCheck x
        AnyValue . Array <$> checkElem v xs
  where
    checkElem :: Value t -> [UValue] -> Either TypeMismatchError [Value t]
    checkElem v []     = Right [v]
    checkElem v (x:xs) = do
        AnyValue vx <- typeCheck x
        Refl <- sameValue v vx
        (v :) <$> checkElem vx xs

rightAny :: Value t -> Either l AnyValue
rightAny = Right . AnyValue

sameValue :: Value a -> Value b -> Either TypeMismatchError (a :~: b)
sameValue Bool{}   Bool{}   = Right Refl
sameValue Int{}    Int{}    = Right Refl
sameValue Float{}  Float{}  = Right Refl
sameValue String{} String{} = Right Refl
sameValue Date{}   Date{}   = Right Refl
sameValue Array{}  Array{}  = Right Refl
sameValue l        r        = Left $ TypeMismatchError
                                         { typeExpected = valueType l
                                         , typeActual   = valueType r
                                         }

-- | Checks whether all elements inside given list of 'AnyValue' have the same
-- type as given 'Value'. Returns list of @Value t@ without given 'Value'.
reifyAnyValues :: Value t -> [AnyValue] -> Either TypeMismatchError [Value t]
reifyAnyValues _ []                 = Right []
reifyAnyValues v (AnyValue av : xs) = sameValue v av >>= \Refl -> (av :) <$> reifyAnyValues v xs
