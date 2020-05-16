{-# LANGUAGE TupleSections #-}

{- |
Copyright: (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Contains TOML-specific combinators for converting between TOML and user data
types.

@since 1.3.0.0
-}

module Toml.Codec.Combinator.Tuple
    ( pair
    , triple
    ) where

import Toml.Codec.Di ((.=))
import Toml.Codec.Types (TomlCodec)


{- | Codec for pair of values. Takes codecs for the first and for the second
values of the pair.

If I have the following @TOML@ entry
@
myPair = { first = 11, second = "eleven"}
@

and want to convert it into the Haskell tuple of two elements, I can use the
following codec:

@
myPairCodec :: 'TomlCodec' ('Int', 'Text')
myPairCodec = flip Toml.'table' \"myPair\" $ Toml.'pair'
    (Toml.'int' \"first\")
    (Toml.'text' \"second\")
@

@since 1.3.0.0
-}
pair :: TomlCodec a -> TomlCodec b -> TomlCodec (a, b)
pair aCodec bCodec = (,)
    <$> aCodec .= fst
    <*> bCodec .= snd
{-# INLINE pair #-}

{- | Codec for triple of values. Takes codecs for the first, second and third
values of the triple.

If I have the following @TOML@ entry
@
myTriple =
    { first = 11
    , second = "eleven"
    , isMyFavourite = true
    }
@

and want to convert it into the Haskell tuple of three elements, I can use the
following codec:

@
myTripleCodec :: 'TomlCodec' ('Int', 'Text', 'Bool')
myTripleCodec = flip Toml.'table' \"myTriple\" $ Toml.'triple'
    (Toml.'int' \"first\")
    (Toml.'text' \"second\")
    (Toml.'bool' \"isMyFavourite\")
@

@since 1.3.0.0
-}
triple :: TomlCodec a -> TomlCodec b -> TomlCodec c -> TomlCodec (a, b, c)
triple aCodec bCodec cCodec = (,,)
    <$> aCodec .= (\(a, _, _) -> a)
    <*> bCodec .= (\(_, b, _) -> b)
    <*> cCodec .= (\(_, _, c) -> c)
{-# INLINE triple #-}
