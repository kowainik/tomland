{- |
Copyright: (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Contains TOML-specific combinators for converting between TOML and user data
types.
-}

module Toml.Codec.Combinator
    ( -- * Basic codecs for primitive values
      -- $primitive
      module Toml.Codec.Combinator.Primitive
      -- ** Time types
      -- $time
    , module Toml.Codec.Combinator.Time

      -- * Combinators for tables
      -- $table
    , module Toml.Codec.Combinator.Table

      -- * Codecs for containers of primitives
      -- ** Lists
      -- $list
    , module Toml.Codec.Combinator.List
      -- ** Sets
      -- $set
    , module Toml.Codec.Combinator.Set
      -- ** Maps
      -- $map
    , module Toml.Codec.Combinator.Map
      -- ** Tuples
      -- $tuple
    , module Toml.Codec.Combinator.Tuple

      -- * Codecs for 'Monoid's
      -- $monoid
    , module Toml.Codec.Combinator.Monoid

      -- * Additional codecs for custom types
      -- $custom
    , module Toml.Codec.Combinator.Custom

      -- * General construction of codecs
      -- $common
    , module Toml.Codec.Combinator.Common
    ) where

import Prelude hiding (all, any, last, map, product, read, sum)

import Toml.Codec.Combinator.Common
import Toml.Codec.Combinator.Custom
import Toml.Codec.Combinator.List
import Toml.Codec.Combinator.Map
import Toml.Codec.Combinator.Monoid
import Toml.Codec.Combinator.Primitive
import Toml.Codec.Combinator.Set
import Toml.Codec.Combinator.Table
import Toml.Codec.Combinator.Time
import Toml.Codec.Combinator.Tuple

{- $primitive
TOML-specific combinators for converting between TOML and Haskell primitive
types, e.g. 'int' \<-\> 'Int', 'byteString' \<-\> 'ByteString'.

See the "Toml.Codec.Combinators.Primitive" module documentation for the overview
table and more examples.
-}

{- $time
TOML-specific combinators for converting between TOML and Haskell date and time
data types. TOML specification describes date and time primitives you
can use in your configuration. @tomland@ provides mapping of those
primitives to types from the @time@ library.

See the "Toml.Codec.Combinators.Time" module documentation for the overview
table and more examples.
-}

{- $table
Combinators for the @TOML@ tables.

See the "Toml.Codec.Combinators.Table" module documentation for more examples.
-}

{- $list
TOML-specific combinators for converting between TOML and Haskell list-like data
types.

See the "Toml.Codec.Combinators.List" module documentation for the overview
table and more examples.
-}

{- $set
TOML-specific combinators for converting between TOML and Haskell set-like data
types.

See the "Toml.Codec.Combinators.Set" module documentation for the overview
table and more examples.
-}

{- $map
TOML-specific combinators for converting between TOML and Haskell map-like data
types.

See the "Toml.Codec.Combinators.Map" module documentation for the overview
table and more examples.
-}

{- $tuple
TOML-specific combinators for converting between TOML and Haskell tuples.
It's recommended to create your custom data types and implement codecs
for them, but if you need to have tuples (e.g. for decoding different
constructors of sum types), you can find codecs from this module
helpful.

See the "Toml.Codec.Combinators.Tuple" module documentation for the overview
table and more examples.
-}

{- $monoid
TOML-specific combinators for converting between TOML and Haskell 'Monoid'
wrapper data types. These codecs are especially handy when you are implementing
the [Partial Options Monoid](https://medium.com/@jonathangfischoff/the-partial-options-monoid-pattern-31914a71fc67)
pattern.

See the "Toml.Codec.Combinators.Monoid" module documentation for the overview
table and more examples.
-}

{- $custom
This module provides additional combinators that could help in the situation
when some additional manipulations for the standard combinators is required.

/For example,/ 'validate' allows to perform some custom validation on the codec
before encoding. And 'enumBounded' is an automatical codec that uses 'Enum' and
'Bounded' instances of the data type only and provides descriptive error
messages at the same time.

See the "Toml.Codec.Combinators.Custom" module documentation for the overview
table and more examples.
-}

{- $common
This module implements common utilities for writing custom codecs
without diving into internal implementation details. Most of the time
you don't need to implement your own codecs and can reuse existing
ones. But if you need something that library doesn't provide, you can
find functions in this module useful.

See the "Toml.Codec.Combinators.Common" module documentation for more examples.
-}
