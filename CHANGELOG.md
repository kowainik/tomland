# Changelog

tomland uses [PVP Versioning][1].
The changelog is available [on GitHub][2].

## 1.1.1.0
* [#206](https://github.com/kowainik/tomland/issues/206):
  Bug fix in parser and additional tests

## 1.1.0.0 — Jul 8, 2019

* [#154](https://github.com/kowainik/tomland/issues/154):
  Implement `Generic` bidirectional codecs
  (by [@chshersh](https://github.com/chshersh)).
* [#145](https://github.com/kowainik/tomland/issues/145):
  Add support for inline table arrays
  (by [@jiegillet](https://github.com/jiegillet)).
* [#195](https://github.com/kowainik/tomland/issues/195):
  Fix an exponential parser behavior for parsing table of arrays
  (by [@jiegillet](https://github.com/jiegillet)).
* [#190](https://github.com/kowainik/tomland/issues/190):
  Add `enumBounded` codec for nullary sum types
  (by [@mxxo](https://github.com/mxxo)).
* [#189](https://github.com/kowainik/tomland/issues/189):
  **Breaking change:** Implement custom table sorting by keys. Also fields of
  the `PrintOptions` data type were renamed according to style guide
  (by [@ramanshah](https://github.com/ramanshah)).

  __Before:__

  ```haskell
  data PrintOptions = PrintOptions
      { shouldSort :: Bool
      , indent     :: Int
      } deriving (Show)
  ```

  __Now:__

  ```haskell
  data PrintOptions = PrintOptions
      { printOptionsSorting :: !(Maybe (Key -> Key -> Ordering))
      , printOptionsIndent  :: !Int
      }
  ```

  __Migration guide:__ If you used `indent` field, use `printOptionsIndent`
  instead. If you used `shouldSort`, use `printOptionsSorting` instead and pass
  `Nothing` instead of `False` or `Just compare` instead of `True`.

## 1.0.1.0 — May 17, 2019

* [#177](https://github.com/kowainik/tomland/issues/177):
  Add a more extensive property generator for `Piece`.
* [#187](https://github.com/kowainik/tomland/issues/187):
  Bump up to `hedgehog-1.0`.
* Support GHC 8.6.5

## 1.0.0 — Jan 14, 2019

* [#13](https://github.com/kowainik/tomland/issues/13):
  Support array of tables.

  + [#131](https://github.com/kowainik/tomland/issues/131):
    Uncommenting `tomlTableArrays` from `TOML`.
  + [#134](https://github.com/kowainik/tomland/issues/134):
    Pretty printer arrays of tables and golden tests.
  + [#143](https://github.com/kowainik/tomland/issues/143):
    Parser for arrays of tables.
  + [#155](https://github.com/kowainik/tomland/issues/155):
    Add `list` and `nonEmpty` combinators for coding lists of custom user types.
  + [#142](https://github.com/kowainik/tomland/issues/142):
    Adding EDSL support for arrays of tables.
  + [#144](https://github.com/kowainik/tomland/issues/144):
    Added tests for arrays of tables.

* [#140](https://github.com/kowainik/tomland/issues/140):
  **Breaking change:** Replace `wrapper` by `diwrap`.

  _Migration guide:_ change `Toml.wrapper Toml.text "foo"` to `Toml.diwrap (Toml.text "foo")`.
* [#152](https://github.com/kowainik/tomland/issues/152):
  **Breaking change:** Removing `mdimap`.

  _Migration guide:_ change `Toml.mdimap showX parseX (Toml.text "foo")` to `Toml.textBy showX parseX "foo"`.
* [#137](https://github.com/kowainik/tomland/issues/137):
  Replace `Maybe` with `Either` in `BiMap`.
* [#174](https://github.com/kowainik/tomland/issues/174):
  Add `_LText` and `lazyText` codecs.
* [#163](https://github.com/kowainik/tomland/issues/163):
   Move all time data types from nested `DateTime` to `Value`.
* [#146](https://github.com/kowainik/tomland/issues/146):
  Allow underscores in floats.
* [#64](https://github.com/kowainik/tomland/issues/64):
  Integer parser doesn't accept leading zeros.
* [#50](https://github.com/kowainik/tomland/issues/50):
   Add property-based tests for encoder and decoder.
* [#119](https://github.com/kowainik/tomland/issues/119):
  Add property-based tests for `BiMap`.
* [#149](https://github.com/kowainik/tomland/issues/149):
  Removing records syntax from `PrefixTree`.

## 0.5.0 — Nov 12, 2018

* [#81](https://github.com/kowainik/tomland/issues/81):
  **Breaking change:** Rename data types.

  _Migration guide:_ rename `Bijection` to `Codec`, `Bi` to `BiCodec` and
  `BiToml` to `TomlCodec`.
* [#82](https://github.com/kowainik/tomland/issues/82):
  **Breaking change:** Remove `maybeT`. Add `dioptional` instead.

  _Migration guide:_ replace `Toml.maybeT Toml.int "foo"` with `Toml.dioptional (Toml.int "foo")`.
* [#95](https://github.com/kowainik/tomland/issues/95):
  **Breaking change:** Swap fields in `BiMap`s for consistency with `lens` package.

  _Migration guide:_ reverse order of composition when using `BiMap`s.
* [#98](https://github.com/kowainik/tomland/issues/98):
  Implement benchmarks for `tomland` and compare with `htoml` and `htoml-megaparsec` libraries.
* [#130](https://github.com/kowainik/tomland/issues/130):
  Added combinators to `Toml.Bi.Combinators`.
* [#115](https://github.com/kowainik/tomland/issues/115):
  Added time combinators to `Toml.BiMap` and `Toml.Bi.Combinators`.
* [#99](https://github.com/kowainik/tomland/issues/99):
  Split `Toml.Parser` file into smaller files.
* [#22](https://github.com/kowainik/tomland/issues/22):
  Report proper type checking error during parsing.
* [#14](https://github.com/kowainik/tomland/issues/14):
  Add support for inline tables parsing.
* [#70](https://github.com/kowainik/tomland/issues/70):
  Add `_TextBy` and `_Read` combinators.
* [#11](https://github.com/kowainik/tomland/issues/11):
  Add `PrintOptions` (sorting, indentation) for pretty printer.
* [#17](https://github.com/kowainik/tomland/issues/17):
  Allow underscores in integers*.
* [#90](https://github.com/kowainik/tomland/issues/90):
  Migrate to megaparsec 7.0.
* [#85](https://github.com/kowainik/tomland/issues/85):
  Add `Date` generator for property-based tests.
* [#88](https://github.com/kowainik/tomland/issues/88):
  Add `Array` generator for property-based tests.
* [#86](https://github.com/kowainik/tomland/issues/86):
  Improve `String` generator for property-based tests.
* [#87](https://github.com/kowainik/tomland/issues/87):
  Improve `Double` generator for property-based tests.
* Add support for GHC 8.6.1.
  Add support for GHC 8.4.4.
  Drop support for GHC 8.0.2.
* [#109](https://github.com/kowainik/tomland/issues/109):
  Add function `decodeToml`.

## 0.4.0

* [#54](https://github.com/kowainik/tomland/issues/54):
  Add support for sum types.
  Rename `Prism` to `BiMap`.
  Rename `bijectionMaker` to `match`.
  Add `string` codec.

## 0.3.1

* [#19](https://github.com/kowainik/tomland/issues/19):
  Add proper parsing of floating point numbers.
* [#15](https://github.com/kowainik/tomland/issues/15):
  Add parsing of multiline strings.
* [#40](https://github.com/kowainik/tomland/issues/40):
  Support full-featured string parser.
* [#18](https://github.com/kowainik/tomland/issues/18):
  Add dates parsing.
* Add useful combinators for `newtype` wrappers.
* [#58](https://github.com/kowainik/tomland/issues/58):
  Add `decodeFile` function.

## 0.3

* [#60](https://github.com/kowainik/tomland/issues/60):
  **Breaking change:** Replace `Valuer` with `Prism`.

  _Migration guide:_ replace any `fooV` with corresponding prism `_Foo`.
* [#66](https://github.com/kowainik/tomland/issues/66):
  **Breaking change:** Introduce consistent names according to Haskell types.

  _Migration guide:_ see issue details to know which names to use.
* [#8](https://github.com/kowainik/tomland/issues/8):
  Create EDSL for easier TOML data type writing.
* [#10](https://github.com/kowainik/tomland/issues/10):
  Add `Semigroup` and `Monoid` instances for `PrefixTree` and `TOML`.
  Add property tests on laws.
* [#20](https://github.com/kowainik/tomland/issues/20):
  Add parsing of hexadecimal, octal, and binary integer numbers.
* [#26](https://github.com/kowainik/tomland/issues/26):
  Implement unit tests for TOML parsers.
  Allow terminating commas inside an array.
  Allow comments before and after any value inside an array.
  Allow keys to be literal strings.

## 0.2.1

* Make `table` parser work with `maybeP`.
* [#39](https://github.com/kowainik/tomland/issues/39):
  Implement `prettyException` function for `DecodeException`.

## 0.2.0

* Switch names for `decode` and `encode` functions.
* [#47](https://github.com/kowainik/tomland/issues/47):
  Rename `dimapBijection` to `dimap`. Introduce `mdimap` combinator.
* [#37](https://github.com/kowainik/tomland/issues/37):
  Add tables support for bidirectional conversion.

## 0.1.0

* [#16](https://github.com/kowainik/tomland/issues/16):
  Add parser for literal strings.
* Add `IsString` instance for `Key` data type.
* [#38](https://github.com/kowainik/tomland/issues/38):
  Add bidirectional converter for array.
* [#21](https://github.com/kowainik/tomland/issues/21):
  Report expected vs. actual type error in parsing.
* [#44](https://github.com/kowainik/tomland/issues/44):
  Add bidirectional converter for `Maybe`.

## 0.0.0

* [#3](https://github.com/kowainik/tomland/issues/3):
  Implement basic TOML parser with `megaparsec`.
* [#7](https://github.com/kowainik/tomland/issues/7):
  Implement type safe version of `Value` type as GADT.
* [#4](https://github.com/kowainik/tomland/issues/4):
  Implement basic pretty-printer.
* [#1](https://github.com/kowainik/tomland/issues/1):
  Implement types representing TOML configuration.
* Initially created.

[1]: https://pvp.haskell.org
[2]: https://github.com/kowainik/tomland/releases
