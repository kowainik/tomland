module Toml.Bi.Code
       ( -- * Types
         BiToml
       , Env
       , St

         -- * Exceptions
       , DecodeException (..)
       , prettyException

         -- * Encode/Decode
       , decode
       , encode
       ) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (Reader, runReader)
import Control.Monad.State (State, execState)
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.Semigroup ((<>))
import Data.Text (Text)

import Toml.Bi.Monad (Bi, Bijection (..))
import Toml.Parser (ParseException (..), parse)
import Toml.PrefixTree (Key (..), unPiece)
import Toml.Printer (prettyToml)
import Toml.Type (TOML (..), TValue, showType)

import qualified Data.Text as Text

-- | Type of exception for converting from 'Toml' to user custom data type.
data DecodeException
    = KeyNotFound Key  -- ^ No such key
    | TableNotFound Key  -- ^ No such table
    | TypeMismatch Key Text TValue  -- ^ Expected type vs actual type
    | ParseError ParseException  -- ^ Exception during parsing
    deriving (Eq, Show)  -- TODO: manual pretty show instances

-- | Converts 'DecodeException' into pretty human-readable text.
prettyException :: DecodeException -> Text
prettyException = \case
    KeyNotFound name -> "Key " <> joinKey name <> " not found"
    TableNotFound name -> "Table [" <> joinKey name <> "] not found"
    TypeMismatch name expected actual -> "Expected type " <> expected <> " for key " <> joinKey name
                                      <> " but got: " <> Text.pack (showType actual)
    ParseError (ParseException msg) -> "Parse error during conversion from TOML to custom user type: \n  " <> msg
  where
    joinKey :: Key -> Text
    joinKey = Text.intercalate "." . map unPiece . toList . unKey

-- | Immutable environment for 'Toml' conversion.
-- This is @r@ type variable in 'Bijection' data type.
type Env = ExceptT DecodeException (Reader TOML)

-- | Mutable context for 'Toml' conversion.
-- This is @w@ type variable in 'Bijection' data type.
type St = State TOML

-- | Specialied 'Bi' type alias for 'Toml' monad. Keeps 'TOML' object either as
-- environment or state.
type BiToml a = Bi Env St a

-- | Convert textual representation of toml into user data type.
decode :: BiToml a -> Text -> Either DecodeException a
decode biToml text = do
    toml <- first ParseError (parse text)
    runReader (runExceptT $ biRead biToml) toml

-- | Convert object to textual representation.
encode :: BiToml a -> a -> Text
encode bi obj = prettyToml $ execState (biWrite bi obj) mempty
