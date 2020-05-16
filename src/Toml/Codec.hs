{- |
Copyright: (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Reexports functions under @Toml.Codec.*@.
-}

module Toml.Codec
    ( -- $types
      module Toml.Codec.Types
      -- $error
    , module Toml.Codec.Error
      -- $code
    , module Toml.Codec.Code
      -- $di
    , module Toml.Codec.Di
      -- $combinator
    , module Toml.Codec.Combinator
      -- $generic
    , module Toml.Codec.Generic
      -- $bimap
    , module Toml.Codec.BiMap
      -- $bimapConversion
    , module Toml.Codec.BiMap.Conversion
    ) where

import Toml.Codec.BiMap
import Toml.Codec.BiMap.Conversion
import Toml.Codec.Code
import Toml.Codec.Combinator
import Toml.Codec.Di
import Toml.Codec.Error
import Toml.Codec.Generic
import Toml.Codec.Types

{- $types
Core codec types, including @Toml@ specialised ones: 'TomlCodec', 'TomlState'
and 'TomlEnv'.
-}

{- $error
Core error types, including 'TomlDecodeError' and 'LoadTomlException'.
-}

{- $code
Contains TOML-specific combinators for converting between TOML and user data
types.
-}

{- $di
Forward and backward mapping functions and combinators (similar to profunctors).
-}

{- $combinator
Contains TOML-specific combinators and codecs for converting between TOML and
user data types.
-}

{- $generic
Automatic TOML codecs using 'GHC.Generics.Generic'.
-}

{- $bimap
'BiMap' type that represents /Tagged Partial Bidirectional Conversion/
between TOML primitives and Haskell types.
-}

{- $bimapConversion
Specific implementations of 'BiMap' between Haskell types and TOML
values.
-}
