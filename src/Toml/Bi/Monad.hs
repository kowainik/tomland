{-# LANGUAGE UndecidableInstances #-}

-- | Contains general underlying monad for bidirectional TOML converion.

module Toml.Bi.Monad
       ( Bijection (..)
       , Bi
       , dimap
       , (<!>)
       , (.=)
       ) where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..))

{- | Monad for bidirectional Toml conversion. Contains pair of functions:

1. How to read value of type @a@ from immutable environment context @r@?
2. How to store value of type @a@ in stateful context @w@?

In practice instead of @r@ we will use some @Reader Toml@ and instead of @w@ we will
use @State Toml@. This approach with the bunch of utility functions allows to
have single description for from/to 'Toml' conversion.

In practice this type will always be used in the following way:

@
type 'Bi' r w a = 'Bijection' r w a a
@

Type parameter @c@ if fictional. Here some trick is used. This trick is
implemented in [codec](http://hackage.haskell.org/package/codec) and
described in more details in [related blog post](https://blog.poisson.chat/posts/2016-10-12-bidirectional-serialization.html).

-}
data Bijection r w c a = Bijection
    { -- | Extract value of type @a@ from monadic context @r@.
      biRead  :: r a

      -- | Store value of type @c@ inside monadic context @w@ and returning
      -- value of type @a@. Type of this function actually should be @a -> w ()@ but with
      -- such type it's impossible to have 'Monad' and other instances.
    , biWrite :: c -> w a
    }

-- | Specialized version of 'Bijection' data type. This type alias is used in practice.
type Bi r w a = Bijection r w a a

instance (Functor r, Functor w) => Functor (Bijection r w c) where
    fmap :: (a -> b) -> Bijection r w c a -> Bijection r w c b
    fmap f bi = Bijection
        { biRead  = f <$> biRead bi
        , biWrite = fmap f . biWrite bi
        }

instance (Applicative r, Applicative w) => Applicative (Bijection r w c) where
    pure :: a -> Bijection r w c a
    pure a = Bijection
        { biRead  = pure a
        , biWrite = \_ -> pure a
        }

    (<*>) :: Bijection r w c (a -> b) -> Bijection r w c a -> Bijection r w c b
    bif <*> bia = Bijection
        { biRead  = biRead bif <*> biRead bia
        , biWrite = \c -> biWrite bif c <*> biWrite bia c
        }

instance (Monad r, Monad w) => Monad (Bijection r w c) where
    (>>=) :: Bijection r w c a -> (a -> Bijection r w c b) -> Bijection r w c b
    bi >>= f = Bijection
        { biRead  = biRead bi >>= \a -> biRead (f a)
        , biWrite = \c -> biWrite bi c >>= \a -> biWrite (f a) c
        }

instance (Alternative r, Alternative w) => Alternative (Bijection r w c) where
    empty :: Bijection r w c a
    empty = Bijection
        { biRead  = empty
        , biWrite = \_ -> empty
        }

    (<|>) :: Bijection r w c a -> Bijection r w c a -> Bijection r w c a
    bi1 <|> bi2 = Bijection
        { biRead  = biRead bi1 <|> biRead bi2
        , biWrite = \c -> biWrite bi1 c <|> biWrite bi2 c
        }

instance (MonadPlus r, MonadPlus w) => MonadPlus (Bijection r w c) where
    mzero = empty
    mplus = (<|>)

infixl 3 <!>
-- | Alternative instance for function arrow but without 'empty'.
(<!>) :: Alternative f => (a -> f x) -> (a -> f x) -> (a -> f x)
f <!> g = \a -> f a <|> g a

{- | This is an instance of 'Profunctor' for 'Bijection'. But since there's no
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
exampleT :: BiToml Example
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
exampleT :: BiToml Example
exampleT = Example
    <$> bool "foo" .= foo
    <*> dimap unEmail Email (str  "bar") .= bar
@
-}
dimap :: (Functor r, Functor w)
      => (c -> d)  -- ^ Mapper for consumer
      -> (a -> b)  -- ^ Mapper for producer
      -> Bijection r w d a  -- ^ Source 'Bijection' object
      -> Bijection r w c b
dimap f g bi = Bijection
  { biRead  = g <$> biRead bi
  , biWrite = fmap g . biWrite bi . f
  }

{- | Operator to connect two operations:

1. How to get field from object?
2. How to write this field to toml?

In code this should be used like this:

@
data Foo = Foo { fooBar :: Int, fooBaz :: String }

foo :: BiToml Foo
foo = Foo
 <$> int "bar" .= fooBar
 <*> str "baz" .= fooBaz
@
-}
infixl 5 .=
(.=) :: Bijection r w field a -> (object -> field) -> Bijection r w object a
bijection .= getter = bijection { biWrite = biWrite bijection . getter }
