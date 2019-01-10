{- | This module reexports all functionality of @tomland@ package. It's
recommended to import this module qualified, like this:

@
__import__ Toml (TomlCodec, (.=))
__import__ __qualified__ Toml
@

Simple @'TomlCodec'@ looks like this:

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

For bigger examples see README.md in the repository:

* [tomland/README.md](https://github.com/kowainik/tomland#tomland)

For the details of the library implementation see blog post:

* [tomland: Bidirectional TOML serialization](TODO)
-}

module Toml
    ( module Toml.Bi
    , module Toml.Parser
    , module Toml.PrefixTree
    , module Toml.Printer
    , module Toml.Type
    ) where

import Toml.Bi
import Toml.Parser
import Toml.PrefixTree
import Toml.Printer
import Toml.Type
