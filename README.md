# tomland

![palm](https://user-images.githubusercontent.com/4276606/51088259-7a777000-176e-11e9-9d76-6be4023c0ac3.png)
[![Build status](https://img.shields.io/travis/kowainik/tomland.svg?logo=travis)](https://travis-ci.org/kowainik/tomland)
[![Hackage](https://img.shields.io/hackage/v/tomland.svg?logo=haskell)](https://hackage.haskell.org/package/tomland)
[![Stackage LTS](http://stackage.org/package/tomland/badge/lts)](http://stackage.org/lts/package/tomland)
[![Stackage Nightly](http://stackage.org/package/tomland/badge/nightly)](http://stackage.org/nightly/package/tomland)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/kowainik/tomland/blob/master/LICENSE)

> “A library is like an island in the middle of a vast sea of ignorance,
> particularly if the library is very tall and the surrounding area has been
> flooded.”
>
> ― Lemony Snicket, Horseradish

Bidirectional TOML serialization. The following blog post has more details about
library design:

* [`tomland`: Bidirectional TOML serialization](https://kowainik.github.io/posts/2019-01-14-tomland)

This README contains a basic usage example of the `tomland` library. All code
below can be compiled and run with the following command:

```
cabal new-run readme
```

## Preamble: imports and language extensions

Since this is a literate haskell file, we need to specify all our language
extensions and imports up front.

```haskell
{-# OPTIONS -Wno-unused-top-binds #-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<|>))
import Control.Category ((>>>))
import Data.Text (Text)
import Toml (TomlBiMap, TomlCodec, (.=))

import qualified Data.Text.IO as TIO
import qualified Toml
```

`tomland` is mostly designed for qualified imports and intended to be imported
as follows:

```haskell ignore
import Toml (TomlCodec, (.=))  -- add 'TomlBiMap' and 'Key' here optionally
import qualified Toml
```

## Data type: parsing and printing

We're going to parse TOML configuration from [`examples/readme.toml`](examples/readme.toml) file.

This static configuration is captured by the following Haskell data type:

```haskell
data Settings = Settings
    { settingsPort        :: !Port
    , settingsDescription :: !Text
    , settingsCodes       :: [Int]
    , settingsMail        :: !Mail
    , settingsUsers       :: ![User]
    }

data Mail = Mail
    { mailHost           :: !Host
    , mailSendIfInactive :: !Bool
    }

data User
    = Admin  Integer  -- id of admin
    | Client Text     -- name of the client
    deriving (Show)

newtype Port = Port Int
newtype Host = Host Text
```

Using `tomland` library, you can write bidirectional converters for these types
using the following guidelines and helper functions:

1. If your fields are some simple basic types like `Int` or `Text` you can just
   use standard codecs like `Toml.int` and `Toml.text`.
2. If you want to parse `newtype`s, use `Toml.diwrap` to wrap parsers for
   underlying `newtype` representation.
3. For parsing nested data types, use `Toml.table`. But this requires to specify
   this data type as TOML table in `.toml` file.
4. If you have lists of custom data types, use `Toml.list`. Such lists are
   represented as array of tables in TOML. If you have lists of primitive types
   like `Int`, `Bool`, `Double`, `Text` or time types, that you can use
   `Toml.arrayOf` and parse arrays of values.
5. `tomland` separates conversion between Haskell types and TOML values from
   matching values by keys. Converters between types and values have type
   `TomlBiMap` and are named with capital letter started with underscore. Main
   type for TOML codecs is called `TomlCodec`. To lift `TomlBiMap` to
   `TomlCodec` you need to use `Toml.match` function.

```haskell
settingsCodec :: TomlCodec Settings
settingsCodec = Settings
    <$> Toml.diwrap (Toml.int  "server.port")       .= settingsPort
    <*> Toml.text              "server.description" .= settingsDescription
    <*> Toml.arrayOf Toml._Int "server.codes"       .= settingsCodes
    <*> Toml.table mailCodec   "mail"               .= settingsMail
    <*> Toml.list  userCodec   "user"               .= settingsUsers

mailCodec :: TomlCodec Mail
mailCodec = Mail
    <$> Toml.diwrap (Toml.text "host") .= mailHost
    <*> Toml.bool "send-if-inactive"   .= mailSendIfInactive

_Admin :: TomlBiMap User Integer
_Admin = Toml.prism Admin $ \case
    Admin i -> Right i
    other   -> Toml.wrongConstructor "Admin" other

_Client :: TomlBiMap User Text
_Client = Toml.prism Client $ \case
    Client n -> Right n
    other    -> Toml.wrongConstructor "Client" other

userCodec :: TomlCodec User
userCodec =
        Toml.match (_Admin >>> Toml._Integer) "id"
    <|> Toml.match (_Client >>> Toml._Text) "name"
```

And now we're ready to parse our TOML and print the result back to see whether
everything is okay.

```haskell
main :: IO ()
main = do
    tomlExample <- TIO.readFile "examples/readme.toml"
    let res = Toml.decode settingsCodec tomlExample
    case res of
        Left err -> print err
        Right settings -> TIO.putStrLn $ Toml.encode settingsCodec settings
```

## Benchmarks and comparison with other libraries

`tomland` is compared with other libraries. Since it uses 2-step approach with
converting text to intermediate AST and only then decoding Haskell type from
this AST, benchmarks are also implemented in a way to reflect this difference.

| Library            | parse :: Text -> AST | transform :: AST -> Haskell |
|--------------------|----------------------|-----------------------------|
| `tomland`          | `305.5 μs`           | `1.280 μs`                  |
| `htoml`            | `852.8 μs`           | `33.37 μs`                  |
| `htoml-megaparsec` | `295.0 μs`           | `33.62 μs`                  |
| `toml-parser`      | `164.6 μs`           | `1.101 μs`                  |

You may see that `tomland` is not the fastest one (though still very fast). But
performance hasn’t been optimized so far and:

1. `toml-parser` doesn’t support the array of tables and because of that it’s
   hardly possible to specify the list of custom data types in TOML with this
   library.
2. `tomland` supports latest TOML spec while `htoml` and `htoml-megaparsec`
   don’t have support for all types, values and formats.
3. `tomland` is the only library that has pretty-printing.
4. `toml-parser` doesn’t have ways to convert TOML AST to custom Haskell types
   and `htoml*` libraries use typeclasses-based approach via `aeson` library.
5. `tomland` is bidirectional :slightly_smiling_face:

## Acknowledgement

Icons made by [Freepik](http://www.freepik.com) from
[www.flaticon.com](https://www.flaticon.com/) is licensed by
[CC 3.0 BY](http://creativecommons.org/licenses/by/3.0/).
