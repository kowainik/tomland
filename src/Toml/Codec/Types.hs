{- |
Copyright: (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Contains general underlying monad for bidirectional conversion.
-}

module Toml.Codec.Types
       ( -- * Toml Codec
         TomlCodec
       , TomlEnv
       , TomlState

         -- * Codec
       , Codec (..)
       ) where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..))
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (Reader)
import Control.Monad.State (State)
import Control.Monad.Trans.Maybe (MaybeT (..))

import Toml.Codec.Error (TomlDecodeError)
import Toml.Type (TOML (..))


-- | Immutable environment for TOML conversion.
type TomlEnv = ExceptT TomlDecodeError (Reader TOML)

{- | Mutable context for TOML conversion.

@
MaybeT (State TOML) a
    = State TOML (Maybe a)
    = TOML -> (Maybe a, TOML)
@
-}
type TomlState = MaybeT (State TOML)

{- | Specialied 'Codec' type alias for bidirectional TOML serialization. Keeps
'TOML' object as both environment and state.
-}
type TomlCodec a = Codec a a

{- | Monad for bidirectional conversion. Contains pair of functions:

1. How to read value of type @a@ from immutable environment context ('TomlEnv')?
2. How to store value of type @a@ in stateful context ('TomlState')?

This approach with the bunch of utility functions allows to
have single description for from/to @TOML@ conversion.

In practice this type will always be used in the following way:

@
type 'TomlCodec' a = 'Codec' a a
@

Type parameter @c@ if fictional. Here some trick is used. This trick is
implemented in the [codec](http://hackage.haskell.org/package/codec) package and
described in more details in related blog post:
<https://blog.poisson.chat/posts/2016-10-12-bidirectional-serialization.html>.
-}
data Codec inT outT = Codec
    { -- | Extract value of type @a@ from monadic context @r@.
      codecRead  :: TomlEnv outT

      {- | Store value of type @c@ inside monadic context 'TomlState' and
      returning value of type @a@. Type of this function actually should be
      @a -> TomlState ()@ but with such type it's impossible to have 'Monad'
      and other instances.
      -}
    , codecWrite :: inT -> TomlState outT
    }

instance Functor (Codec inT) where
    fmap :: (outA -> outB) -> Codec inT outA -> Codec inT outB
    fmap f codec = Codec
        { codecRead  = f <$> codecRead codec
        , codecWrite = fmap f . codecWrite codec
        }
    {-# INLINE fmap #-}

instance Applicative (Codec inT) where
    pure :: out -> Codec inT out
    pure a = Codec
        { codecRead  = pure a
        , codecWrite = \_ -> pure a
        }
    {-# INLINE pure #-}

    (<*>) :: Codec inT (outA -> outB) -> Codec inT outA -> Codec inT outB
    codecf <*> codeca = Codec
        { codecRead  = codecRead codecf <*> codecRead codeca
        , codecWrite = \c -> codecWrite codecf c <*> codecWrite codeca c
        }
    {-# INLINE (<*>) #-}

instance Monad (Codec inT) where
    (>>=) :: Codec inT outA -> (outA -> Codec inT outB) -> Codec inT outB
    codec >>= f = Codec
        { codecRead  = codecRead codec >>= \a -> codecRead (f a)
        , codecWrite = \c -> codecWrite codec c >>= \a -> codecWrite (f a) c
        }
    {-# INLINE (>>=) #-}

instance Alternative (Codec inT) where
    empty :: Codec inT out
    empty = Codec
        { codecRead  = empty
        , codecWrite = const empty
        }
    {-# INLINE empty #-}

    (<|>) :: Codec inT out -> Codec inT out -> Codec inT out
    codec1 <|> codec2 = Codec
        { codecRead  = codecRead codec1 <|> codecRead codec2
        , codecWrite = \c -> codecWrite codec1 c <|> codecWrite codec2 c
        }
    {-# INLINE (<|>) #-}

instance MonadPlus (Codec inT) where
    mzero :: Codec inT out
    mzero = empty
    {-# INLINE mzero #-}

    mplus :: Codec inT out -> Codec inT out -> Codec inT out
    mplus = (<|>)
    {-# INLINE mplus #-}
