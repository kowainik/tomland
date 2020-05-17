{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE Rank2Types          #-}

{- |
Copyright: (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Implementation of /Tagged Partial Bidirectional Isomorphism/. This
module contains the 'BiMap' type that represents conversion between
two types with the possibility of failure.

See "Toml.Codec.BiMap.Conversion" for examples of 'BiMap' with
specific types. The 'BiMap' concept is general and is not specific to
TOML, but in this package most usages of 'BiMap' are between TOML
values and Haskell values.
-}

module Toml.Codec.BiMap
    ( -- * 'BiMap' concept
      BiMap (..)
    , invert
    , iso
    , prism

      -- * TOML 'BiMap'
      -- ** Type
    , TomlBiMap
      -- ** Error
    , TomlBiMapError (..)
    , wrongConstructor
    , prettyBiMapError
      -- ** Smart constructors
    , mkAnyValueBiMap
      -- ** Internals
    , tShow
    ) where

import Control.DeepSeq (NFData)
import Control.Monad ((>=>))
import Data.Bifunctor (first)
import Data.Text (Text)
import GHC.Generics (Generic)

import Toml.Type.AnyValue (AnyValue (..), MatchError (..))
import Toml.Type.Value (TValue (..), Value (..))

import qualified Control.Category as Cat
import qualified Data.Text as T


{- | Partial bidirectional isomorphism. @BiMap a b@ contains two function:

1. @a -> Either e b@
2. @b -> Either e a@

If you think of types as sets then this data type can be illustrated by the
following picture:

![bimap-type](https://user-images.githubusercontent.com/4276606/50770531-b6a36000-1298-11e9-9528-caae87951d2a.png)

'BiMap' also implements 'Cat.Category' typeclass. And this instance can be described
clearly by this illustration:

![bimap-cat](https://user-images.githubusercontent.com/4276606/50771234-13a01580-129b-11e9-93da-6c5dd0f7f160.png)

@since 0.4.0
-}
data BiMap e a b = BiMap
    { forward  :: a -> Either e b
    , backward :: b -> Either e a
    }

-- | @since 0.4.0
instance Cat.Category (BiMap e) where
    id :: BiMap e a a
    id = BiMap Right Right
    {-# INLINE id #-}

    (.) :: BiMap e b c -> BiMap e a b -> BiMap e a c
    bc . ab = BiMap
        { forward  =  forward ab >=>  forward bc
        , backward = backward bc >=> backward ab
        }
    {-# INLINE (.) #-}

{- | Inverts bidirectional mapping.

@since 0.4.0
-}
invert :: BiMap e a b -> BiMap e b a
invert (BiMap f g) = BiMap g f
{-# INLINE invert #-}

{- | Creates 'BiMap' from isomorphism. Can be used in the following way:

@
__newtype__ Even = Even Integer
__newtype__ Odd  = Odd  Integer

succEven :: Even -> Odd
succEven (Even n) = Odd (n + 1)

predOdd :: Odd -> Even
predOdd (Odd n) = Even (n - 1)

_EvenOdd :: 'BiMap' e Even Odd
_EvenOdd = 'iso' succEven predOdd
@

@since 0.4.0
-}
iso :: (a -> b) -> (b -> a) -> BiMap e a b
iso f g = BiMap (Right . f) (Right . g)
{-# INLINE iso #-}

{- | Creates 'BiMap' from prism-like pair of functions. This combinator can be
used to create 'BiMap' for custom sum types like this:

@
__data__ User
    = Admin  Integer  -- id of admin
    | Client Text     -- name of the client
    __deriving__ (Show)

_Admin :: 'TomlBiMap' User Integer
_Admin = Toml.'prism' Admin $ \\__case__
    Admin i -> Right i
    other   -> Toml.'wrongConstructor' \"Admin\" other

_Client :: 'TomlBiMap' User Text
_Client = Toml.'prism' Client $ \\__case__
    Client n -> Right n
    other    -> Toml.'wrongConstructor' \"Client\" other
@

@since 0.4.0
-}
prism
    :: (field -> object)
    -- ^ Constructor
    -> (object -> Either error field)
    -- ^ Match object to either error or field
    -> BiMap error object field
prism review preview = BiMap preview (Right . review)
{-# INLINE prism #-}

----------------------------------------------------------------------------
-- TOML BiMap
----------------------------------------------------------------------------

{- | 'BiMap' specialized to TOML error.

@since 1.0.0
-}
type TomlBiMap = BiMap TomlBiMapError

{- | Type of errors for TOML 'BiMap'.

@since 1.0.0
-}
data TomlBiMapError
    = WrongConstructor -- ^ Error for cases with wrong constructors. For
                       -- example, you're trying to convert 'Left' but
                       -- bidirectional converter expects 'Right'.
        !Text          -- ^ Expected constructor name
        !Text          -- ^ Actual value
    | WrongValue       -- ^ Error for cases with wrong values
        !MatchError    -- ^ Information about failed matching
    | ArbitraryError   -- ^ Arbitrary textual error
        !Text          -- ^ Error message
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

{- | Converts 'TomlBiMapError' into pretty human-readable text.

@since 1.0.0
-}
prettyBiMapError :: TomlBiMapError -> Text
prettyBiMapError = \case
    WrongConstructor expected actual -> T.unlines
        [ "Invalid constructor"
        , "  * Expected: " <> expected
        , "  * Actual:   " <> actual
        ]
    WrongValue (MatchError expected actual) -> T.unlines
        [ "Invalid constructor"
        , "  * Expected: " <> tShow expected
        , "  * Actual:   " <> tShow actual
        ]
    ArbitraryError text  -> text

{- | Helper to construct WrongConstuctor error.

@since 1.0.0
-}
wrongConstructor
    :: Show a
    => Text  -- ^ Name of the expected constructor
    -> a     -- ^ Actual value
    -> Either TomlBiMapError b
wrongConstructor constructor x = Left $ WrongConstructor constructor (tShow x)

tShow :: Show a => a -> Text
tShow = T.pack . show
{-# INLINE tShow #-}

----------------------------------------------------------------------------
--  BiMaps for value
----------------------------------------------------------------------------

{- | Smart constructor for 'BiMap' from a Haskell value (some
primitive like 'Int' or 'Text') to 'AnyValue'.

@since 0.4.0
-}
mkAnyValueBiMap
    :: forall a (tag :: TValue)
    .  (forall (t :: TValue) . Value t -> Either MatchError a)
    -- ^ Haskell type exctractor from 'Value'
    -> (a -> Value tag)
    -- ^ Convert Haskell type back to 'Value'
    -> TomlBiMap a AnyValue
mkAnyValueBiMap matchValue toValue = BiMap
    { forward  = Right . toAnyValue
    , backward = fromAnyValue
    }
  where
    toAnyValue :: a -> AnyValue
    toAnyValue = AnyValue . toValue

    fromAnyValue :: AnyValue -> Either TomlBiMapError a
    fromAnyValue (AnyValue value) = first WrongValue $ matchValue value
