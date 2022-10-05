{- |
Module                  : Toml.Codec
Copyright               : (c) 2018-2022 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

This module provides utilities for implementing and using
bidirectional TOML codecs. The concept of bidirectional conversion in
@tomland@ has two parts: __'TomlBiMap'__ and __'TomlCodec'__.

== General TOML description

In the following TOML

@
name = "foo"
@

we call @name@ as __key__ and @"foo"@ as __value__.

== 'TomlBiMap'

'TomlBiMap' provides a bidirectional conversion between
__TOML values__ and Haskell primitive values. TOML specification
defines some primitive values you can use in key-value pairs
(e.g. /integer/, /string/, /local time/). 'TomlBiMap' provides a way
to convert between TOML primitives and Haskell values. 'TomlBiMap'
doesn't know anything about TOML keys.

== 'TomlCodec'

'TomlCodec' describes how to convert in both ways between a single or
multiple key-value pairs and Haskell types. @tomland@ provides basic
primitives for decoding and encoding single key-value pairs, but also
a way to compose multiple 'TomlCodec's into a single one. So, if you
have a custom data type, that has several fields or several
constructors, you need to define 'TomlCodec' for your data type.

== Encoding and decoding

If you have a type like @User@ then @userCodec :: 'TomlCodec' User@ is
an object that describes how to 'encode' a value of type @User@ to
TOML and 'decode' TOML to a value of type @User@.

* To TOML: @'encode' userCodec someUser@
* From TOML: @'decode' userCodec someToml@

== Naming conventions

@tomland@ uses the following naming conventions (and encourages
library users to follow them as well):

* __\_SomeName__: for 'TomlBiMap' (e.g. '_Int', '_Text', '_Array')
* __someName__: for basic 'TomlCodec's (e.g. 'int', 'text', 'arrayOf')
* __someNameCodec__: for user defined codecs for custom types (e.g. @userCodec@, @configCodec@, @serverCodec@)

@since 1.3.0.0
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
