Change log
==========

tomland uses [PVP Versioning][1].
The change log is available [on GitHub][2].

0.5.0
=====

* [#115](https://github.com/kowainik/tomland/issues/115):
  Added time combinators to `Toml.BiMap` and `Toml.Bi.Combinators`.
* [#22](https://github.com/kowainik/tomland/issues/22):
  Report proper type checking error during parsing.
* [#95](https://github.com/kowainik/tomland/issues/95)
  Swap fields in BiMaps for consistency with `lens` package.
* [#70](https://github.com/kowainik/tomland/issues/70)
  Add `_TextBy` and `_Show` combinators
* [#11](https://github.com/kowainik/tomland/issues/11)
  Add `PrintOptions` (sorting, indentation) for pretty printer.
* [#17](https://github.com/kowainik/tomland/issues/17)
  Allow underscores in integers*.
* [#90](https://github.com/kowainik/tomland/issues/90):
  Migrate to megaparsec 7.0
* [#81](https://github.com/kowainik/tomland/issues/81):
  **Important:** Rename data types.
  _Migration guide:_ rename `Bijection` to `Codec`, `Bi` to `BiCodec` and
  `BiToml` to `TomlCodec`.
* [#85](https://github.com/kowainik/tomland/issues/85):
  Add `Date` generator for property-based tests.
* [#88](https://github.com/kowainik/tomland/issues/88):
  Add `Array` generator for property-based tests.
* [#86](https://github.com/kowainik/tomland/issues/86):
  Improve `String` generator for property-based tests.
* [#87](https://github.com/kowainik/tomland/issues/87):
  Improve `Double` generator for property-based tests.
* Add support for GHC 8.6.1.
  Drop support for GHC 8.0.2.
* [#82](https://github.com/kowainik/tomland/issues/82):
  Remove `maybeT`. Add `diOptional`.
* [#109](https://github.com/kowainik/tomland/issues/109):
  Add function `decodeToml`.

0.4.0
=====

* [#54](https://github.com/kowainik/tomland/issues/54):
  Add support for sum types.
  Rename `Prism` to `BiMap`.
  Rename `bijectionMaker` to `match`.
  Add `string` codec.

0.3.1
=====
* [#19](https://github.com/kowainik/tomland/issues/19):
  Add proper parsing of floating point numbers.
* [#15](https://github.com/kowainik/tomland/issues/15):
  Add parsing of multiline strings
* [#40](https://github.com/kowainik/tomland/issues/40):
  Support full-featured string parser
* [#18](https://github.com/kowainik/tomland/issues/18):
  Add dates parsing.
* Add useful combinators for `newtype` wrappers.
* [#58](https://github.com/kowainik/tomland/issues/58):
  Add `decodeFile` function.

0.3
=====

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
* **Breaking change:** [#60](https://github.com/kowainik/tomland/issues/60):
  Replace `Valuer` with `Prism`.

  _Migration guide:_ replace any `fooV` with corresponding prism `_Foo`.
* **Breaking change:** [#66](https://github.com/kowainik/tomland/issues/66):
  Introduce consistent names according to Haskell types.

  _Migration guide:_ see issue details to know which names to use.

0.2.1
=====
* Make `table` parser work with `maybeP`.
* [#39](https://github.com/kowainik/tomland/issues/39):
  Implement `prettyException` function for `DecodeException`.

0.2.0
=====
* Switch names for `decode` and `encode` functions.
* [#47](https://github.com/kowainik/tomland/issues/47):
  Rename `dimapBijection` to `dimap`. Introduce `mdimap` combinator.
* [#37](https://github.com/kowainik/tomland/issues/37):
  Add tables support for bidirectional conversion.

0.1.0
=====
* [#16](https://github.com/kowainik/tomland/issues/16):
  Add parser for literal strings.
* Add `IsString` instance for `Key` data type.
* [#38](https://github.com/kowainik/tomland/issues/38):
  Add bidirectional converter for array.
* [#21](https://github.com/kowainik/tomland/issues/21):
  Report expected vs. actual type error in parsing.
* [#44](https://github.com/kowainik/tomland/issues/44):
  Add bidirectional converter for `Maybe`.

0.0.0
=====
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
