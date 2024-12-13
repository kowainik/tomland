{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{- |
Module                  : Toml.Codec.Types
Copyright               : (c) 2018-2022 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Contains general underlying monad for bidirectional conversion.

@since 1.3.0.0
-}

module Toml.Codec.Types
       ( -- * Toml Codec
         TomlCodec
         -- * Toml Environment
       , TomlEnv
         -- * Toml State
       , TomlState (..)
       , eitherToTomlState

         -- * Codec
       , Codec (..)

         -- * Function alternative
       , (<!>)
       ) where

import Control.Applicative (Alternative (..), liftA2)
import Control.Monad.State (MonadState (..))
import Data.Bifunctor (first)
import Validation (Validation (..))

import Toml.Codec.Error (TomlDecodeError)
import Toml.Type (TOML (..))


{- | Immutable environment for TOML conversion.

@since 1.3.0.0
-}
type TomlEnv a = TOML -> Validation [TomlDecodeError] a


{- | Specialized 'Codec' type alias for bidirectional TOML serialization. Keeps
'TOML' object as both environment and state.

@since 0.5.0
-}
type TomlCodec a = Codec a a

{- | Monad for bidirectional conversion. Contains a pair of functions:

1. How to read a value of type @o@ (out) from an immutable environment context
('TomlEnv')?
2. How to store a value of type @i@ (in) in a stateful context ('TomlState')?

This approach, along with a bunch of utility functions, allows us to
have a single description for @TOML@ serialization + deserialization.

In practice this type will always be used in the following way:

@
type 'TomlCodec' a = 'Codec' a a
@

However, we need to represent @i@ separately from @o@ in order to implement
instances like 'Alternative', which requires the type parameter to be covariant,
but @i@ is contravariant. This trick is inspired by the
[codec](http://hackage.haskell.org/package/codec) package and
described in more details in this blog post:
<https://blog.poisson.chat/posts/2016-10-12-bidirectional-serialization.html>.

@since 0.0.0
-}
data Codec i o = Codec
    { -- | Extract value of type @o@ from monadic context 'TomlEnv'.
      codecRead  :: TomlEnv o
      -- | Store value of type @i@ inside monadic context 'TomlState'.
    , codecWrite :: i -> TomlState ()
    }

-- | @since 0.0.0
instance Functor (Codec i) where
    fmap :: (oA -> oB) -> Codec i oA -> Codec i oB
    fmap f codec = Codec
        { codecRead  = fmap f . codecRead codec
        , codecWrite = codecWrite codec
        }
    {-# INLINE fmap #-}

-- | @since 0.0.0
instance Applicative (Codec i) where
    pure :: o -> Codec i o
    pure a = Codec
        { codecRead  = \_ -> Success a
        , codecWrite = \_ -> pure ()
        }
    {-# INLINE pure #-}

    (<*>) :: Codec i (oA -> oB) -> Codec i oA -> Codec i oB
    codecf <*> codeca = Codec
        { codecRead  = liftA2 (<*>) (codecRead codecf) (codecRead codeca)
        , codecWrite = \c -> codecWrite codecf c *> codecWrite codeca c
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

{- | Mutable context for TOML conversion.
We are introducing our own implementation of state with 'MonadState' instance due
to some limitations in the design connected to the usage of State.

This newtype is equivalent to the following transformer:

@
MaybeT (State TOML)
@

@since 1.3.0.0
-}
newtype TomlState a = TomlState
    { unTomlState :: TOML -> (Maybe a, TOML)
    }

-- | @since 1.3.0.0
instance Functor TomlState where
    fmap :: (a -> b) -> TomlState a -> TomlState b
    fmap f TomlState{..} = TomlState (first (fmap f) . unTomlState)
    {-# INLINE fmap #-}

    (<$) :: a -> TomlState b -> TomlState a
    a <$ TomlState{..} = TomlState (first (fmap (const a)) . unTomlState)
    {-# INLINE (<$) #-}

-- | @since 1.3.0.0
instance Applicative TomlState where
    pure :: a -> TomlState a
    pure a = TomlState (Just a,)
    {-# INLINE pure #-}

    (<*>) :: TomlState (a -> b) -> TomlState a -> TomlState b
    tsF <*> tsA = TomlState $ \t ->
        let (mF, tF) = unTomlState tsF t
            (mA, tA) = unTomlState tsA tF
        in (mF <*> mA , tA)
    {-# INLINE (<*>) #-}

-- | @since 1.3.0.0
instance Alternative TomlState where
    empty :: TomlState a
    empty = TomlState (Nothing,)
    {-# INLINE empty #-}

    (<|>) :: TomlState a -> TomlState a -> TomlState a
    ts1 <|> ts2 = TomlState $ \t -> let (m1, t1) = unTomlState ts1 t in case m1 of
        Nothing -> unTomlState ts2 t
        Just _  -> (m1, t1)
    {-# INLINE (<|>) #-}

-- | @since 1.3.0.0
instance Monad TomlState where
    return :: a -> TomlState a
    return = pure
    {-# INLINE return #-}

    (>>=) :: TomlState a -> (a -> TomlState b) -> TomlState b
    tsA >>= f = TomlState $ \t -> let (mA, newT) = unTomlState tsA t in case mA of
        Nothing -> (Nothing, newT)
        Just a  -> unTomlState (f a) newT
    {-# INLINE (>>=) #-}

-- | @since 1.3.0.0
instance (s ~ TOML) => MonadState s TomlState where
    state :: (TOML -> (a, TOML)) -> TomlState a
    state f = TomlState (first Just . f)
    {-# INLINE state #-}

    get :: TomlState TOML
    get = TomlState (\t -> (Just t, t))
    {-# INLINE get #-}

    put :: TOML -> TomlState ()
    put t = TomlState (\_ -> (Just (), t))
    {-# INLINE put #-}

{- | Transform 'Either' into 'TomlState'.

@since 1.3.0.0
-}
eitherToTomlState :: Either e a -> TomlState a
eitherToTomlState e = TomlState (either (const Nothing) Just e,)
