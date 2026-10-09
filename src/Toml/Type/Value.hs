{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

{- |
Module                  : Toml.Type.Value
Copyright               : (c) 2018-2022 Kowainik
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
       , array
       , eqValueList
       , valueType

         -- * Existential wrapper
       , AnyValue (..)

         -- * TOML AST
       , TOML (..)
       , Entry (..)
       , TableKind (..)

         -- * Type checking
       , TypeMismatchError (..)
       , sameValue
       ) where

import Control.DeepSeq (NFData (..), rnf)
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Time (Day, LocalTime, TimeOfDay, ZonedTime, zonedTimeToUTC)
import Data.Type.Equality ((:~:) (..))
import GHC.Generics (Generic)

import Toml.Type.Key (Piece)

import qualified Data.HashMap.Strict as HashMap



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
    | TTable
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

    {- | Array of values. Since TOML 1.0.0 the elements of an array may have
      different types, so the elements are wrapped in 'AnyValue'. Use 'array'
      to build an array from a list of values of the same type.

@
arr1 = [ 1, 2, 3 ]
arr2 = [ "red", "yellow", "green" ]
arr3 = [ [ 1, 2 ], [3, 4, 5] ]
arr4 = [ "all", \'strings\', """are the same""", \'\'\'type\'\'\']
arr5 = [ [ 1, 2 ], ["a", "b", "c"] ]
arr6 = [ 1, 2.0, "three", { four = 4 } ]
@

    @since 1.4.0.0: the elements are 'AnyValue's instead of @Value t@.
    -}
    Array  :: [AnyValue] -> Value 'TArray

    {- | Table used as a value, i.e. an inline table inside an array:

@
points = [ { x = 1, y = 2 }, "origin" ]
@

    An inline table that is the direct value of a key is stored as a table
    entry of the enclosing 'TOML' (see 'Toml.Type.TOML.Entry'), so this
    constructor only appears inside arrays after parsing.

    @since 1.4.0.0
    -}
    Table  :: TOML -> Value 'TTable

{- | Builds an 'Array' from values of the same type.

@
__>>>__ array [Integer 1, Integer 2]
Array [Integer 1,Integer 2]
@

@since 1.4.0.0
-}
array :: [Value t] -> Value 'TArray
array = Array . map AnyValue
{-# INLINE array #-}

{- | Existential wrapper for 'Value'.

@since 0.0.0
-}
data AnyValue = forall (t :: TValue) . AnyValue (Value t)

instance Show AnyValue where
    show (AnyValue v) = show v

instance Eq AnyValue where
    (AnyValue val1) == (AnyValue val2) = case sameValue val1 val2 of
        Right Refl -> val1 == val2
        Left _     -> False

instance NFData AnyValue where
    rnf (AnyValue val) = rnf val

{- | How a table was introduced in the document. The spec treats all four the
same way for lookup purposes, but they differ in which later definitions may
extend them, so the parser keeps the distinction for validation and the
printer uses it to reproduce the original layout.

@since 1.4.0.0
-}
data TableKind
    = HeaderTable
      -- ^ Defined by a @[table]@ header. Can get sub-tables and arrays of
      -- tables through further headers.
    | ImplicitTable
      -- ^ Created because a sub-table was defined before it, e.g. @a@ in
      -- @[a.b]@. A later @[a]@ header turns it into a 'HeaderTable'.
    | DottedTable
      -- ^ Created by a dotted key, e.g. @a@ in @a.b = 1@. Can get more
      -- dotted keys within the same table and sub-tables through headers,
      -- but cannot be reopened with a header of its own.
    | InlineTable
      -- ^ Defined as an inline table @a = { ... }@. Closed: nothing can be
      -- added to it afterwards.
    deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

{- | One entry of a 'TOML' table, stored under a single key 'Piece'.

@since 1.4.0.0
-}
data Entry
    = EValue !AnyValue
      -- ^ @key = value@ (including inline tables inside arrays, see
      -- 'Toml.Type.Value.Table')
    | ETable !TableKind !TOML
      -- ^ A sub-table, however it was introduced
    | ETableArray !(NonEmpty TOML)
      -- ^ An array of tables defined with @[[key]]@ headers. An inline array
      -- of inline tables, @key = [ {..}, {..} ]@, is an 'EValue' instead.
    deriving stock (Show, Generic)
    deriving anyclass (NFData)

{- | Equality of entries ignores the 'TableKind': a table is the same table no
matter whether it was written with a header, with dotted keys or inline.
-}
instance Eq Entry where
    EValue a       == EValue b       = a == b
    ETable _ a     == ETable _ b     = a == b
    ETableArray a  == ETableArray b  = a == b
    _              == _              = False

{- | Represents TOML configuration value: a table whose entries are indexed
by one key 'Piece' each. Every level of nesting is one piece deep, so the
dotted key @a.b.c = 1@, the headers @[a.b]@ with @c = 1@ and the inline table
@a = { b = { c = 1 } }@ all produce the same structure.

For example, this

@
TOML $ HashMap.fromList
    [ ("title", EValue $ AnyValue $ Text "TOML example")
    , ("example", ETable ImplicitTable $ TOML $ HashMap.fromList
          [ ("owner", ETable HeaderTable $ TOML $ HashMap.fromList
                [ ("name", EValue $ AnyValue $ Text "Kowainik") ]
            )
          ]
      )
    ]
@

corresponds to this TOML document

@
title = "TOML example"

[example.owner]
  name = "Kowainik"
@

The 'Eq' instance ignores 'TableKind's.

@since 1.4.0.0: the three-field record is replaced by a single map.
-}
data TOML = TOML
    { unTOML :: !(HashMap Piece Entry)
    } deriving stock (Show, Eq, Generic)
      deriving anyclass (NFData)

{- | Left-biased union: values of the first 'TOML' win, tables are merged
recursively (keeping the kind of the first one), arrays of tables are
concatenated. When the same key holds entries of different kinds, a value
wins over a table, and a table wins over an array of tables, whatever the
order of the operands; this keeps '<>' associative.

@since 0.3
-}
instance Semigroup TOML where
    (<>) :: TOML -> TOML -> TOML
    TOML a <> TOML b = TOML $ HashMap.unionWith mergeEntry a b
      where
        mergeEntry :: Entry -> Entry -> Entry
        mergeEntry (ETable k t1)    (ETable _ t2)    = ETable k (t1 <> t2)
        mergeEntry (ETableArray a1) (ETableArray a2) = ETableArray (a1 <> a2)
        mergeEntry e1 e2
            | rank e2 < rank e1 = e2
            | otherwise         = e1

        rank :: Entry -> Int
        rank = \case
            EValue _      -> 0
            ETable _ _    -> 1
            ETableArray _ -> 2

-- | @since 0.3
instance Monoid TOML where
    mempty :: TOML
    mempty = TOML mempty
    {-# INLINE mempty #-}

    mappend :: TOML -> TOML -> TOML
    mappend = (<>)
    {-# INLINE mappend #-}

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
    rnf (Table n)   = rnf n

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
    (Array a1)   == (Array a2)   = a1 == a2
    (Table t1)   == (Table t2)   = t1 == t2

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
valueType (Table _)   = TTable

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
sameValue Table{}   Table{}   = Right Refl
sameValue l         r         = Left $ TypeMismatchError
                                         { typeExpected = valueType l
                                         , typeActual   = valueType r
                                         }
