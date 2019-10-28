{- |
Copyright: (c) 2018-2019 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

This module reexports all functionality of @tomland@ package. It's
recommended to import this module qualified, like this:

@
__import__ Toml (TomlCodec, (.=))
__import__ __qualified__ Toml
@

Simple @'TomlCodec'@ could be written in the following way:

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
    ( module Toml.Bi
    , module Toml.Generic
    , module Toml.Parser
    , module Toml.PrefixTree
    , module Toml.Printer
    , module Toml.Type
    ) where

import Toml.Bi
import Toml.Generic
import Toml.Parser
import Toml.PrefixTree
import Toml.Printer
import Toml.Type
