{- |
Copyright: (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

This module reexports all functionality of the @tomland@ package. It's
recommended to import this module qualified, like this:

@
__import__ "Toml" ('TomlCodec', '(.=)')
__import__ __qualified__ "Toml"
@

Simple @'TomlCodec'@ for a Haskell value, that can be decoded from
TOML or encoded as TOML, could be written in the following way:

@
__data__ User = User
    { userName :: Text
    , userAge  :: Int
    }

userCodec :: 'TomlCodec' User
userCodec = User
    \<$\> Toml.'text' "name" '.=' userName
    \<*\> Toml.'int'  "age"  '.=' userAge
@

For more detailed examples see README.md in the repository:

* [tomland/README.md](https://github.com/kowainik/tomland#tomland)

For the details of the library implementation see blog post:

* [tomland: Bidirectional TOML serialization](https://kowainik.github.io/posts/2019-01-14-tomland)
-}

module Toml
    ( -- $codec
      module Toml.Codec
      -- $type
    , module Toml.Type
      -- $parser
    , module Toml.Parser
    ) where

import Toml.Codec
import Toml.Parser
import Toml.Type

{- $codec
Main types and functions to implement TOML codecs. This module
provides high-level API of @tomland@ library.
-}

{- $type
Low-level implementation details of types that power @tomland@. The
"Toml.Type" module contains types to repesent TOML AST and basic
functions to work with it.
-}

{- $parser
Parser for types, defined in "Toml.Type". This modules contains
low-level functions to parse 'TOML' from text. If you want to convert
between Haskell types and TOML representation, use functions from
"Toml.Codec".
-}
