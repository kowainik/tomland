module Toml.Bi.Code
       ( -- * Types
         TomlCodec
       , Env
       , St

         -- * Exceptions
       , DecodeException (..)
       , LoadTomlException (..)
       , prettyException

         -- * Encode/Decode
       , decode
       , decodeFile
       , encode
       ) where

import Control.Exception (Exception, throwIO)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (Reader, runReader)
import Control.Monad.State (State, execState)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.Semigroup (Semigroup (..))
import Data.Text (Text)

import Toml.Bi.Monad (BiCodec, Codec (..))
import Toml.Parser (ParseException (..), parse)
import Toml.PrefixTree (Key (..), unPiece)
import Toml.Printer (pretty)
import Toml.Type (TOML (..), TValue, showType)

import qualified Data.Text as Text
import qualified Data.Text.IO as TIO

-- | Type of exception for converting from 'Toml' to user custom data type.
data DecodeException
    = TrivialError
    | KeyNotFound Key  -- ^ No such key
    | TableNotFound Key  -- ^ No such table
    | TypeMismatch Key Text TValue  -- ^ Expected type vs actual type
    | ParseError ParseException  -- ^ Exception during parsing
    deriving (Eq, Show)  -- TODO: manual pretty show instances

instance Semigroup DecodeException where
    TrivialError <> e = e
    e <> _ = e

instance Monoid DecodeException where
    mempty = TrivialError
    mappend = (<>)

-- | Converts 'DecodeException' into pretty human-readable text.
prettyException :: DecodeException -> Text
prettyException = \case
    TrivialError -> "Using 'empty' parser"
    KeyNotFound name -> "Key " <> joinKey name <> " not found"
    TableNotFound name -> "Table [" <> joinKey name <> "] not found"
    TypeMismatch name expected actual -> "Expected type " <> expected <> " for key " <> joinKey name
                                      <> " but got: " <> Text.pack (showType actual)
    ParseError (ParseException msg) -> "Parse error during conversion from TOML to custom user type: \n  " <> msg
  where
    joinKey :: Key -> Text
    joinKey = Text.intercalate "." . map unPiece . toList . unKey

-- | Immutable environment for 'Toml' conversion.
-- This is @r@ type variable in 'Codec' data type.
type Env = ExceptT DecodeException (Reader TOML)

{- | Mutable context for 'Toml' conversion.
This is @w@ type variable in 'Codec' data type.

@
MaybeT (State TOML) a
    = State TOML (Maybe a)
    = TOML -> (Maybe a, TOML)
@
-}
type St = MaybeT (State TOML)

-- | Specialied 'Bi' type alias for 'Toml' monad. Keeps 'TOML' object either as
-- environment or state.
type TomlCodec a = BiCodec Env St a

-- | Convert textual representation of toml into user data type.
decode :: TomlCodec a -> Text -> Either DecodeException a
decode codec text = do
    toml <- first ParseError (parse text)
    runReader (runExceptT $ codecRead codec) toml

-- | Convert object to textual representation.
encode :: TomlCodec a -> a -> Text
encode codec obj = pretty $ execState (runMaybeT $ codecWrite codec obj) mempty

-- | File loading error data type.
data LoadTomlException = LoadTomlException FilePath Text

instance Show LoadTomlException where
    show (LoadTomlException filePath msg) = "Couldnt parse file " ++ filePath ++ ": " ++ show msg

instance Exception LoadTomlException

-- | Decode a value from a file. In case of parse errors, throws 'LoadTomlException'.
decodeFile :: (MonadIO m) => TomlCodec a -> FilePath -> m a
decodeFile codec filePath = liftIO $
    (decode codec <$> TIO.readFile filePath) >>= errorWhenLeft
  where
    errorWhenLeft :: Either DecodeException a -> IO a
    errorWhenLeft (Left e)   = throwIO $ LoadTomlException filePath $ prettyException e
    errorWhenLeft (Right pc) = pure pc
