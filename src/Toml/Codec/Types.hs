{- |
Copyright: (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Contains general underlying monad for bidirectional conversion.

@since 1.3.0.0
-}

module Toml.Codec.Types
       ( -- * Toml Codec
         TomlCodec
       , TomlEnv
       , TomlState

         -- * Codec
       , Codec (..)

         -- * Function alternative
       , (<!>)
       ) where

import Control.Applicative (Alternative (..), liftA2)
import Control.Monad.State (State)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Validation (Validation (..))

import Toml.Codec.Error (TomlDecodeError)
import Toml.Type (TOML (..))


{- | Immutable environment for TOML conversion.

@since 1.3.0.0
-}
type TomlEnv a = TOML -> Validation [TomlDecodeError] a

{- | Mutable context for TOML conversion.

@
MaybeT (State TOML) a
    = State TOML (Maybe a)
    = TOML -> (Maybe a, TOML)
@

@since 1.3.0.0
-}
type TomlState = MaybeT (State TOML)

{- | Specialied 'Codec' type alias for bidirectional TOML serialization. Keeps
'TOML' object as both environment and state.

@since 0.5.0
-}
type TomlCodec a = Codec a a

{- | Monad for bidirectional conversion. Contains pair of functions:

1. How to read value of type @o@ (out) from immutable environment context
('TomlEnv')?
2. How to store a value of type @i@ (in) in stateful context ('TomlState') and
return a value of type @o@?

This approach with the bunch of utility functions allows to
have single description for from/to @TOML@ conversion.

In practice this type will always be used in the following way:

@
type 'TomlCodec' a = 'Codec' a a
@

Type parameter @i@ if fictional. Here some trick is used. This trick is
implemented in the [codec](http://hackage.haskell.org/package/codec) package and
described in more details in related blog post:
<https://blog.poisson.chat/posts/2016-10-12-bidirectional-serialization.html>.

@since 0.0.0
-}
data Codec i o = Codec
    { -- | Extract value of type @o@ from monadic context 'TomlEnv'.
      codecRead  :: TomlEnv o

      {- | Store value of type @i@ inside monadic context 'TomlState' and
      returning value of type @o@. Type of this function actually should be
      @o -> TomlState ()@ but with such type it's impossible to have 'Monad'
      and other instances.
      -}
    , codecWrite :: i -> TomlState o
    }

-- | @since 0.0.0
instance Functor (Codec i) where
    fmap :: (oA -> oB) -> Codec i oA -> Codec i oB
    fmap f codec = Codec
        { codecRead  = fmap f . codecRead codec
        , codecWrite = fmap f . codecWrite codec
        }
    {-# INLINE fmap #-}

-- | @since 0.0.0
instance Applicative (Codec i) where
    pure :: o -> Codec i o
    pure a = Codec
        { codecRead  = \_ -> Success a
        , codecWrite = \_ -> pure a
        }
    {-# INLINE pure #-}

    (<*>) :: Codec i (oA -> oB) -> Codec i oA -> Codec i oB
    codecf <*> codeca = Codec
        { codecRead  = liftA2 (<*>) (codecRead codecf) (codecRead codeca)
        , codecWrite = \c -> codecWrite codecf c <*> codecWrite codeca c
        }
    {-# INLINE (<*>) #-}

instance Alternative (Codec i) where
    empty :: Codec i o
    empty = Codec
        { codecRead  = \_ -> empty
        , codecWrite = \_ -> empty
        }
    {-# INLINE empty #-}

    (<|>) :: Codec i o -> Codec i o -> Codec i o
    codec1 <|> codec2 = Codec
        { codecRead  = codecRead codec1 <!> codecRead codec2
        , codecWrite = \c -> codecWrite codec1 c <|> codecWrite codec2 c
        }
    {-# INLINE (<|>) #-}

-- | Alternative instance for function arrow but without 'empty'.
infixl 3 <!>
(<!>) :: Alternative f => (a -> f x) -> (a -> f x) -> (a -> f x)
f <!> g = \a -> f a <|> g a
{-# INLINE (<!>) #-}
