{- |
Copyright: (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Reexports functions under @Toml.Codec.*@.

* __"Toml.Bi.Map"__: contains implementation of tagged bidirectional
  isomorphisms.
* __"Toml.Bi.Monad"__: core abstraction for bidirectional conversion based on
  profunctor monads.
* __"Toml.Codec.Combinators"__: TOML specific combinators based on 'Codec' type
  from "Toml.Bi.Monad".
* __"Toml.Codec.Code"__: conversion functions like 'decode' and 'encode'.
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
      -- $combinators
    , module Toml.Codec.Combinators
    ) where

import Toml.Codec.Code
import Toml.Codec.Combinators
import Toml.Codec.Di
import Toml.Codec.Error
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

{- $combinators
Contains TOML-specific combinators and codecs for converting between TOML and
user data types.
-}
