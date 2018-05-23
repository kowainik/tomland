Change log
==========

tomland uses [PVP Versioning][1].
The change log is available [on GitHub][2].

0.2.0
=====
* Switch names for `decode` and `encode` functions.
* [#47](https://github.com/kowainik/tomland/issues/47):
  Rename `dimapBijection` to `dimap`. Introduce `mdimap` combinator.

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
