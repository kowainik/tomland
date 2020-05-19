# tomland

![palm](https://user-images.githubusercontent.com/4276606/51088259-7a777000-176e-11e9-9d76-6be4023c0ac3.png)

[![GitHub CI](https://github.com/kowainik/tomland/workflows/CI/badge.svg)](https://github.com/kowainik/tomland/actions)
[![AppVeyor CI](https://ci.appveyor.com/api/projects/status/github/kowainik/tomland?branch=master&svg=true)](https://ci.appveyor.com/project/kowainik/tomland)
[![Hackage](https://img.shields.io/hackage/v/tomland.svg?logo=haskell)](https://hackage.haskell.org/package/tomland)
[![Stackage LTS](http://stackage.org/package/tomland/badge/lts)](http://stackage.org/lts/package/tomland)
[![Stackage Nightly](http://stackage.org/package/tomland/badge/nightly)](http://stackage.org/nightly/package/tomland)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/kowainik/tomland/blob/master/LICENSE)

> “A library is like an island in the middle of a vast sea of ignorance,
> particularly if the library is very tall and the surrounding area has been
> flooded.”

> ― Lemony Snicket, Horseradish

`tomland` is a Haskell library for _Bidirectional TOML
Serialization_. It provides the composable interface for implementing
[TOML](https://github.com/toml-lang/toml) codecs. If you want to use
TOML as a configuration for your tool or application, you can use
`tomland` to easily convert in both ways between textual TOML
representation and Haskell types.

✍️ `tomland` supports [TOML spec version 0.5.0](https://github.com/toml-lang/toml/wiki#v050-compliant).

The following blog post has more details about the library design and
internal implementation details:

* [`tomland`: Bidirectional TOML Serialization](https://kowainik.github.io/posts/2019-01-14-tomland)

This README contains a basic usage example of the `tomland` library. All code
below can be compiled and run with the following command:

```
cabal run readme
```

## Preamble: imports and language extensions

Since this is a literate haskell file, we need to specify all our language
extensions and imports up front.

```haskell
{-# OPTIONS -Wno-unused-top-binds #-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<|>))
import Data.Text (Text)
import Data.Time (Day)
import Toml (TomlCodec, (.=))

import qualified Data.Text.IO as TIO
import qualified Toml
```

`tomland` is designed for qualified imports and intended to be imported
as follows:

```haskell ignore
import Toml (TomlCodec, (.=))  -- add 'TomlBiMap' and 'Key' here optionally
import qualified Toml
```

## Data type: parsing and printing

We're going to parse TOML configuration from
[`examples/readme.toml`](examples/readme.toml) file. The configuration
contains the following description of our data:

```toml
server.port        = 8080
server.codes       = [ 5, 10, 42 ]
server.description = """
This is production server.
Don't touch it!
"""

[mail]
    host = "smtp.gmail.com"
    send-if-inactive = false

[[user]]
   guestId = 42

[[user]]
   guestId = 114

[[user]]
    login = "Foo Bar"
    createdAt = 2020-05-19
```

The above static configuration describes `Settings` for some
server. It has several top-level fields, a table with the name `mail`
and an array of tables with the name `user` that stores list of
different types of users.

We can model such TOML using the following Haskell data types:

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
    = Guest !Integer  -- id of guest
    | Registered !RegisteredUser  -- login and createdAt of registered user

data RegisteredUser = RegisteredUser
    { registeredUserLogin     :: !Text
    , registeredUserCreatedAt :: !Day
    }

newtype Port = Port Int
newtype Host = Host Text
```

Using the `tomland` library, you can write bidirectional converters for these types
with the following guidelines and helper functions:

1. If your fields are some simple primitive types like `Int` or `Text` you can just
   use standard codecs like `Toml.int` and `Toml.text`.
2. If you want to parse `newtype`s, use `Toml.diwrap` to wrap parsers for
   underlying `newtype` representation.
3. For parsing nested data types, use `Toml.table`. But it requires to specify
   this data type as TOML table in the `.toml` file.
4. If you have lists of custom data types, use `Toml.list`. Such lists are
   represented as _array of tables_ in TOML. If you have lists of the primitive types
   like `Int`, `Bool`, `Double`, `Text` or time types, that you can use
   `Toml.arrayOf` and parse arrays of values.
5. If you have sets of custom data types, use `Toml.set` or `Toml.HashSet`. Such
   sets are represented as array of tables in TOML.
6. For parsing sum types, use `Toml.dimatch`. This requires writing matching functions
   for the constructors of the sum type.
7. `tomland` separates conversion between Haskell types and TOML values from
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

matchGuest :: User -> Maybe Integer
matchGuest = \case
   Guest i -> Just i
   _ -> Nothing

matchRegistered :: User -> Maybe RegisteredUser
matchRegistered = \case
   Registered u -> Just u
   _ -> Nothing

userCodec :: TomlCodec User
userCodec =
        Toml.dimatch matchGuest      Guest      (Toml.integer "guestId")
    <|> Toml.dimatch matchRegistered Registered registeredUserCodec

registeredUserCodec :: TomlCodec RegisteredUser
registeredUserCodec = RegisteredUser
    <$> Toml.text "login"     .= registeredUserLogin
    <*> Toml.day  "createdAt" .= registeredUserCreatedAt
```

And now we are ready to parse our TOML and print the result back to see whether
everything is okay.

```haskell
main :: IO ()
main = do
    tomlRes <- Toml.decodeFileEither settingsCodec "examples/readme.toml"
    case tomlRes of
        Left errs      -> TIO.putStrLn $ Toml.prettyTomlDecodeErrors errs
        Right settings -> TIO.putStrLn $ Toml.encode settingsCodec settings
```

## Benchmarks and comparison with other libraries

You can find benchmarks of the `tomland` library in the following repository:

* [kowainik/toml-benchmarks](https://github.com/kowainik/toml-benchmarks)

Since `tomland` uses 2-step approach with converting text to
intermediate AST and only then decoding Haskell type from this AST,
benchmarks are also implemented in a way to reflect this difference.

| Library            | parse :: Text -> AST | transform :: AST -> Haskell |
|--------------------|----------------------|-----------------------------|
| `tomland`          | `305.5 μs`           | `1.280 μs`                  |
| `htoml`            | `852.8 μs`           | `33.37 μs`                  |
| `htoml-megaparsec` | `295.0 μs`           | `33.62 μs`                  |
| `toml-parser`      | `164.6 μs`           | `1.101 μs`                  |

In addition to the above numbers, `tomland` has several features that
make it unique:

1. `tomland` is the only Haskell library that has pretty-printing.
2. `tomland` is compatible with the latest TOML spec while other libraries are not.
3. `tomland` is bidirectional, which means that your encoding and
   decoding are consistent with each other by construction.
4. `tomland` provides abilities for `Generic` and `DerivingVia`
   deriving out-of-the-box.
5. Despite being the fastest, `toml-parser` doesn’t support the array
   of tables and because of that it’s hardly possible to specify the list
   of custom data types in TOML with this library. In addition,
   `toml-parser` doesn’t have ways to convert TOML AST to custom
   Haskell types and `htoml*` libraries use typeclasses-based approach
   via `aeson` library.

## Acknowledgement

Icons made by [Freepik](http://www.freepik.com) from [www.flaticon.com](https://www.flaticon.com/) is licensed by [CC 3.0 BY](http://creativecommons.org/licenses/by/3.0/).
