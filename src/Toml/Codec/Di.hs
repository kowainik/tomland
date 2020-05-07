{- |
Copyright: (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Forward and backward mapping functions and combinators (similar to profunctors).
-}

module Toml.Codec.Di
    ( dimap
    , dioptional
    , diwrap
    , dimatch
    , (<!>)
    , (.=)

    ) where

import Control.Applicative (Alternative (..), optional)
import Data.Coerce (Coercible, coerce)

import Toml.Codec.Types (Codec (..), TomlCodec)


{- | This is an instance of @Profunctor@ for 'Codec'. But since there's no
@Profunctor@ type class in @base@ or package with no dependencies (and we don't
want to bring extra dependencies) this instance is implemented as a single
top-level function.

Useful when you want to parse @newtype@s. For example, if you had data type like
this:

@
__data__ Example = Example
    { foo :: Bool
    , bar :: Text
    }
@

Bidirectional TOML converter for this type will look like this:

@
exampleCodec :: TomlCodec Example
exampleCodec = Example
    \<$\> Toml.bool "foo" '.=' foo
    \<*\> Toml.text "bar" '.=' bar
@

Now if you change your type in the following way:

@
__newtype__ Email = Email { unEmail :: Text }

__data__ Example = Example
    { foo :: Bool
    , bar :: Email
    }
@

you need to patch your TOML codec like this:

@
exampleCodec :: TomlCodec Example
exampleCodec = Example
    \<$\> Toml.bool "foo" '.=' foo
    \<*\> 'dimap' unEmail Email (Toml.text "bar") '.=' bar
@
-}
dimap
    :: (b -> a)    -- ^ Mapper for consumer
    -> (a -> b)  -- ^ Mapper for producer
    -> TomlCodec a  -- ^ Source 'Codec' object
    -> TomlCodec b  -- ^ Target 'Codec' object
dimap f g codec = Codec
    { codecRead  = g <$> codecRead codec
    , codecWrite = fmap g . codecWrite codec . f
    }
{-# INLINE dimap #-}

{- | Bidirectional converter for @Maybe a@ values. For example, given the data
type:

@
__data__ Example = Example
    { foo :: Bool
    , bar :: Maybe Int
    }
@

the TOML codec will look like

@
exampleCodec :: TomlCodec Example
exampleCodec = Example
    \<$\> Toml.bool "foo" '.=' foo
    \<*\> 'dioptional' (Toml.int "bar") '.=' bar
@
-}
dioptional
    :: TomlCodec a
    -> TomlCodec (Maybe a)
dioptional Codec{..} = Codec
    { codecRead  = optional codecRead
    , codecWrite = traverse codecWrite
    }
{-# INLINE dioptional #-}

{- | Combinator used for @newtype@ wrappers. For example, given the data types:

@
__newtype__ N = N Int

__data__ Example = Example
    { foo :: Bool
    , bar :: N
    }
@

the TOML codec can look like

@
exampleCodec :: TomlCodec Example
exampleCodec = Example
    \<$\> Toml.bool "foo" '.=' foo
    \<*\> 'diwrap' (Toml.int "bar") '.=' bar
@
-}
diwrap
    :: forall b a .
       (Coercible a b)
    => TomlCodec a
    -> TomlCodec b
diwrap = dimap coerce coerce
{-# INLINE diwrap #-}

{- | Bidirectional converter for @sum types@. For example, given the data
type:

@
__data__ Example
    = Foo Int
    | Bar Bool Int
@

the TOML codec will look like

@
matchFoo :: Example -> Maybe Int
matchFoo (Foo num) = Just num
matchFoo _         = Nothing

matchBar :: Example -> Maybe (Bool, Int)
matchBar (Bar b num) = Just (b, num)
matchBar _           = Nothing

barCodec :: TomlCodec (Bool, Int)
barCodec = (,)
    \<$\> Toml.bool "a" '.=' fst
    \<*\> Toml.int "b" '.=' snd

exampleCodec :: TomlCodec Example
exampleCodec =
    dimatch matchFoo Foo (Toml.int "foo")
    \<|\> dimatch matchBar (uncurry Bar) (Toml.table barCodec "bar")
@

@since 1.2.0.0
-}
dimatch
    :: (b -> Maybe a)  -- ^ Mapper for consumer
    -> (a -> b)     -- ^ Mapper for producer
    -> TomlCodec a  -- ^ Source 'Codec' object
    -> TomlCodec b  -- ^ Target 'Codec' object
dimatch match ctor codec = Codec
    { codecRead = ctor <$> codecRead codec
    , codecWrite = \c -> case match c of
        Nothing -> empty
        Just d  -> ctor <$> codecWrite codec d
    }
{-# INLINE dimatch #-}

{- | Operator to connect two operations:

1. How to get field from object?
2. How to write this field to toml?

In code this should be used like this:

@
__data__ Foo = Foo
    { fooBar :: Int
    , fooBaz :: String
    }

fooCodec :: TomlCodec Foo
fooCodec = Foo
    \<$\> Toml.int "bar" '.=' fooBar
    \<*\> Toml.str "baz" '.=' fooBaz
@
-}
infixl 5 .=
(.=) :: Codec field a -> (object -> field) -> Codec object a
codec .= getter = codec { codecWrite = codecWrite codec . getter }
{-# INLINE (.=) #-}

-- | Alternative instance for function arrow but without 'empty'.
infixl 3 <!>
(<!>) :: Alternative f => (a -> f x) -> (a -> f x) -> (a -> f x)
f <!> g = \a -> f a <|> g a
{-# INLINE (<!>) #-}
