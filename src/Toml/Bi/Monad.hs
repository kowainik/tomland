-- | Contains general underlying monad for bidirectional TOML converion.

module Toml.Bi.Monad
       ( Bijection (..)
       , Bi
       , (.=)
       ) where

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
