{- | Contains specification of TOML via Haskell ADT. -}

module Toml.Type
       ( TOML (..)
       , Key (..)
       , TableId (..)
       , Value (..)
       , DateTime (..)
       ) where

import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Time (Day, LocalTime, TimeOfDay, ZonedTime)

{- | Key of value in @key = val@ pair.
-}
newtype Key = Key
    { unKey :: Text
    } deriving (Eq, Ord, Hashable)

{- | Name of table. Stored as 'NonEmpty' list of components.

@
[table.name]
  key1 = "val1"
  key2 = "val2"
@
-}
newtype TableId = TableId
    { unTableId :: NonEmpty Text
    } deriving (Eq, Ord, Hashable)

-- TODO: describe how some TOML document will look like with this type
{- | Represents TOML configuration value. -}
data TOML = TOML
    { tomlPairs       :: HashMap Key     Value
    , tomlTables      :: HashMap TableId TOML
    , tomlTableArrays :: HashMap TableId (NonEmpty TOML)
    -- TODO: I don't really like the above structure, probably something better should be used
    }

-- TODO: examples are copy-pasted from TOML specification. Probably most of them
-- will be moved into parsing module in future.
-- | Value in @key = value@ pair.
data Value
      {- | Boolean value:

@
bool1 = true
bool2 = false
@
      -}
    = Bool !Bool

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

    | Int !Integer
      {- | Floating point number:

@
flt1 = -3.1415   # fractional
flt2 = 1e6       # exponent
flt3 = 6.626e-34 # both
flt4 = 9_224_617.445_991_228_313
@
      -}
    | Float !Double

      {- | String value:

@
key = "value"
bare_key = "value"
bare-key = "value"
@
      -}
    | String !Text

      -- | Date-time. See documentation for 'DateTime' type.
    | Date !DateTime

      {- | Array of values. According to TOML specification all values in array
      should have the same type. This is not guaranteed statically yet.

@
arr1 = [ 1, 2, 3 ]
arr2 = [ "red", "yellow", "green" ]
arr3 = [ [ 1, 2 ], [3, 4, 5] ]
arr4 = [ "all", 'strings', """are the same""", '''type''']
arr5 = [ [ 1, 2 ], ["a", "b", "c"] ]

arr6 = [ 1, 2.0 ] # INVALID
@
      -}
    | Array ![Value]

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
