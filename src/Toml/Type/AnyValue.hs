{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}

module Toml.Type.AnyValue
       ( AnyValue (..)
       , MatchError (..)
       , TomlBiMapError (..)
       , reifyAnyValues
       , toMArray

         -- * Matching
       , liftMatch
       , matchBool
       , matchInteger
       , matchDouble
       , matchText
       , matchDate
       , matchArray

       -- * Util
       , leftWrongValue
       , typeName
       , toTxt
       ) where

import Control.DeepSeq (NFData, rnf)
import Data.Text (Text)
import Data.Type.Equality ((:~:) (..))
import Data.Typeable (Proxy (..), Typeable, typeRep)

import Toml.Type.Value (DateTime, TValue (..), TypeMismatchError, Value (..), sameValue)

import qualified Data.Text as T


-- | Existential wrapper for 'Value'.
data AnyValue = forall (t :: TValue) . AnyValue (Value t)

instance Show AnyValue where
    show (AnyValue v) = show v

instance Eq AnyValue where
    (AnyValue val1) == (AnyValue val2) = case sameValue val1 val2 of
        Right Refl -> val1 == val2
        Left _     -> False

instance NFData AnyValue where
    rnf (AnyValue val) = rnf val

----------------------------------------------------------------------------
-- Matching functions for values
----------------------------------------------------------------------------

-- | Extract 'Bool' from 'Value'.
matchBool :: Value t -> Either TomlBiMapError Bool
matchBool (Bool b) = Right b
matchBool value    = leftWrongValue value

-- | Extract 'Integer' from 'Value'.
matchInteger :: Value t -> Either TomlBiMapError Integer
matchInteger (Integer n) = Right n
matchInteger value       = leftWrongValue value

-- | Extract 'Double' from 'Value'.
matchDouble :: Value t -> Either TomlBiMapError Double
matchDouble (Double f) = Right f
matchDouble value      = leftWrongValue value

-- | Extract 'Text' from 'Value'.
matchText :: Value t -> Either TomlBiMapError Text
matchText (Text s) = Right s
matchText value    = leftWrongValue value

-- | Extract 'DateTime' from 'Value'.
matchDate :: Value t -> Either TomlBiMapError DateTime
matchDate (Date d) = Right d
matchDate value    = leftWrongValue value

-- | Extract list of elements of type @a@ from array.
matchArray :: (AnyValue -> Either TomlBiMapError a) -> Value t -> Either TomlBiMapError [a]
matchArray matchValue (Array a) = mapM (liftMatch matchValue) a
matchArray _          value     = leftWrongValue value

liftMatch :: (AnyValue -> Either TomlBiMapError a) -> (Value t -> Either TomlBiMapError a)
liftMatch fromAnyValue = fromAnyValue . AnyValue

-- | Checks whether all elements inside given list of 'AnyValue' have the same
-- type as given 'Value'. Returns list of @Value t@ without given 'Value'.
reifyAnyValues :: Value t -> [AnyValue] -> Either TypeMismatchError [Value t]
reifyAnyValues _ []                 = Right []
reifyAnyValues v (AnyValue av : xs) = sameValue v av >>= \Refl -> (av :) <$> reifyAnyValues v xs

-- | Function for creating 'Array' from list of 'AnyValue'.
toMArray :: [AnyValue] -> Either TomlBiMapError (Value 'TArray)
toMArray [] = Right $ Array []
toMArray (AnyValue x : xs) = case reifyAnyValues x xs of
    Left _     -> leftWrongValue x
    Right vals -> Right $ Array (x : vals)

---------------------------------------------------------------------------
-- Value mismatch error and TomlBiMapError
---------------------------------------------------------------------------

data MatchError = MatchError
    { valueExpected :: Text
    , valueActual   :: AnyValue
    } deriving (Eq, Show)

data TomlBiMapError
    -- Error for cases with wrong constructors.
    -- For example, you're trying to convert 'Left' but bidirectional converter expects 'Right'
    = WrongConstructor
        Text                -- ^ Expected constructor name
        Text                -- ^ Actual Value; TODO: use Show here?
    | WrongValue MatchError -- ^ Error for cases with wrong values.
    | ArbitraryError Text
    deriving Show

----------------------------------------------------------------------
-- Useful functions
----------------------------------------------------------------------

toTxt :: Show a => a -> Text
toTxt = T.pack . show

-- | Expacted value
typeName :: forall a . Typeable a => Text
typeName = toTxt $ typeRep $ Proxy @a

-- | Left error part with WrongValue.
leftWrongValue :: forall (t :: TValue) b . Value t -> Either TomlBiMapError b
leftWrongValue = Left . WrongValue . MatchError (typeName @TValue) . AnyValue
