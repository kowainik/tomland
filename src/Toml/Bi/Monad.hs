{-# LANGUAGE UndecidableInstances #-}

-- | Contains general underlying monad for bidirectional TOML converion.

module Toml.Bi.Monad
       ( Codec (..)
       , BiCodec
       , dimap
       , dioptional
       , (<!>)
       , (.=)
       ) where

import Control.Applicative (Alternative (..), optional)
import Control.Monad (MonadPlus (..))

{- | Monad for bidirectional conversion. Contains pair of functions:

1. How to read value of type @a@ from immutable environment context @r@?
2. How to store value of type @a@ in stateful context @w@?

In practice instead of @r@ we will use some @Reader Toml@ and instead of @w@ we will
use @State Toml@. This approach with the bunch of utility functions allows to
have single description for from/to 'Toml' conversion.

In practice this type will always be used in the following way:

@
type 'BiCodec' r w a = 'Codec' r w a a
@

Type parameter @c@ if fictional. Here some trick is used. This trick is
implemented in [codec](http://hackage.haskell.org/package/codec) and
described in more details in [related blog post](https://blog.poisson.chat/posts/2016-10-12-bidirectional-serialization.html).

-}
data Codec r w c a = Codec
    { -- | Extract value of type @a@ from monadic context @r@.
      codecRead  :: r a

      -- | Store value of type @c@ inside monadic context @w@ and returning
      -- value of type @a@. Type of this function actually should be @a -> w ()@ but with
      -- such type it's impossible to have 'Monad' and other instances.
    , codecWrite :: c -> w a
    }

-- | Specialized version of 'Codec' data type. This type alias is used in practice.
type BiCodec r w a = Codec r w a a

instance (Functor r, Functor w) => Functor (Codec r w c) where
    fmap :: (a -> b) -> Codec r w c a -> Codec r w c b
    fmap f codec = Codec
        { codecRead  = f <$> codecRead codec
        , codecWrite = fmap f . codecWrite codec
        }

instance (Applicative r, Applicative w) => Applicative (Codec r w c) where
    pure :: a -> Codec r w c a
    pure a = Codec
        { codecRead  = pure a
        , codecWrite = \_ -> pure a
        }

    (<*>) :: Codec r w c (a -> b) -> Codec r w c a -> Codec r w c b
    codecf <*> codeca = Codec
        { codecRead  = codecRead codecf <*> codecRead codeca
        , codecWrite = \c -> codecWrite codecf c <*> codecWrite codeca c
        }

instance (Monad r, Monad w) => Monad (Codec r w c) where
    (>>=) :: Codec r w c a -> (a -> Codec r w c b) -> Codec r w c b
    codec >>= f = Codec
        { codecRead  = codecRead codec >>= \a -> codecRead (f a)
        , codecWrite = \c -> codecWrite codec c >>= \a -> codecWrite (f a) c
        }

instance (Alternative r, Alternative w) => Alternative (Codec r w c) where
    empty :: Codec r w c a
    empty = Codec
        { codecRead  = empty
        , codecWrite = \_ -> empty
        }

    (<|>) :: Codec r w c a -> Codec r w c a -> Codec r w c a
    codec1 <|> codec2 = Codec
        { codecRead  = codecRead codec1 <|> codecRead codec2
        , codecWrite = \c -> codecWrite codec1 c <|> codecWrite codec2 c
        }

instance (MonadPlus r, MonadPlus w) => MonadPlus (Codec r w c) where
    mzero = empty
    mplus = (<|>)

-- | Alternative instance for function arrow but without 'empty'.
infixl 3 <!>
(<!>) :: Alternative f => (a -> f x) -> (a -> f x) -> (a -> f x)
f <!> g = \a -> f a <|> g a

{- | This is an instance of 'Profunctor' for 'Codec'. But since there's no
@Profunctor@ type class in @base@ or package with no dependencies (and we don't
want to bring extra dependencies) this instance is implemented as a single
top-level function.

Useful when you want to parse @newtype@s. For example, if you had data type like this:

@
data Example = Example
    { foo :: Bool
    , bar :: Text
    }
@

toml bidirectional converter for this type will look like this:

@
exampleT :: TomlCodec Example
exampleT = Example
    <$> bool "foo" .= foo
    <*> str  "bar" .= bar
@

Now if you change your time in the following way:

@
newtype Email = Email { unEmail :: Text }

data Example = Example
    { foo :: Bool
    , bar :: Email
    }
@

you need to patch your toml parser like this:

@
exampleT :: TomlCodec Example
exampleT = Example
    <$> bool "foo" .= foo
    <*> dimap unEmail Email (str "bar") .= bar
@
-}
dimap :: (Functor r, Functor w)
      => (c -> d)  -- ^ Mapper for consumer
      -> (a -> b)  -- ^ Mapper for producer
      -> Codec r w d a  -- ^ Source 'Codec' object
      -> Codec r w c b
dimap f g codec = Codec
  { codecRead  = g <$> codecRead codec
  , codecWrite = fmap g . codecWrite codec . f
  }

-- | Bidirectional converter for @Maybe smth@ values.
dioptional :: (Alternative r, Applicative w) => Codec r w c a -> Codec r w (Maybe c) (Maybe a)
dioptional Codec{..} = Codec
    { codecRead = optional codecRead
    , codecWrite = traverse codecWrite
    }

{- | Operator to connect two operations:

1. How to get field from object?
2. How to write this field to toml?

In code this should be used like this:

@
data Foo = Foo { fooBar :: Int, fooBaz :: String }

foo :: TomlCodec Foo
foo = Foo
 <$> int "bar" .= fooBar
 <*> str "baz" .= fooBaz
@
-}
infixl 5 .=
(.=) :: Codec r w field a -> (object -> field) -> Codec r w object a
codec .= getter = codec { codecWrite = codecWrite codec . getter }
