{-# OPTIONS -Wno-unused-top-binds #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia    #-}

module Main (main) where

import Control.Applicative ((<|>))
import Control.Arrow ((>>>))
import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import Data.ByteString (ByteString)
import Data.IntSet (IntSet)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (fromGregorian)
import GHC.Generics (Generic)

import Toml (TomlCodec, TomlParseError (..), pretty, (.=), (<!>))
import Toml.Type (TOML (..), Value (..))
import Toml.Type.Edsl (mkToml, table, (=:))
import Toml.Codec.Generic (TomlTable (..), stripTypeNameCodec, HasCodec (..), ByteStringAsBytes (..))

import qualified Data.Text.IO as TIO

import qualified Toml


newtype TestInside = TestInside { unInside :: Text }

insideCodec :: TomlCodec TestInside
insideCodec = Toml.dimap unInside TestInside $ Toml.text "inside"

data User = User
    { userName :: !Text
    , userAge  :: !Int
    } deriving stock (Eq, Ord, Generic)
      deriving anyclass (Hashable)

userCodec :: TomlCodec User
userCodec = User
    <$> Toml.text "name" .= userName
    <*> Toml.int  "age"  .= userAge

newtype N = N
    { unN :: Text
    }

data ColorScheme
    = Light
    | Dark
    | HighContrast
    deriving stock (Show, Read, Enum, Bounded)

data UserStatus
    = Registered Text Text
    | Anonymous Text

matchRegistered :: UserStatus -> Maybe (Text, Text)
matchRegistered (Registered username password) = Just (username, password)
matchRegistered _                              = Nothing

matchAnonymous :: UserStatus -> Maybe Text
matchAnonymous (Anonymous username) = Just username
matchAnonymous _                    = Nothing

userPassCodec :: TomlCodec (Text, Text)
userPassCodec = Toml.pair
    (Toml.text "username")
    (Toml.text "password")

userStatusCodec :: TomlCodec UserStatus
userStatusCodec =
    Toml.dimatch matchRegistered (uncurry Registered) (Toml.table userPassCodec "testStatus")
    <|> Toml.dimatch matchAnonymous Anonymous (Toml.text "testStatus")

data Colour
   = Hex Text
   | RGB Rgb

matchHex :: Colour -> Maybe Text
matchHex = \case
    Hex t -> Just t
    _ -> Nothing

matchRgb :: Colour -> Maybe Rgb
matchRgb = \case
    RGB rgb -> Just rgb
    _ -> Nothing

colourCodec :: Toml.Key -> TomlCodec Colour
colourCodec key =
        Toml.dimatch matchHex Hex (Toml.text key)
    <|> Toml.dimatch matchRgb RGB (Toml.table rgbCodec key)

data Rgb = Rgb
    { rgbRed   :: Int
    , rgbGreen :: Int
    , rgbBlue  :: Int
    }

rgbCodec :: TomlCodec Rgb
rgbCodec = Rgb
    <$> Toml.int "red"   .= rgbRed
    <*> Toml.int "green" .= rgbGreen
    <*> Toml.int "blue"  .= rgbBlue

data Test = Test
    { testB      :: !Bool
    , testI      :: !Int
    , testF      :: !Double
    , testS      :: !Text
    , testA      :: ![Text]
    , testNE     :: !(NonEmpty Text)
    , testNET    :: !(NonEmpty Int)
    , testM      :: !(Maybe Bool)
    , testX      :: !TestInside
    , testY      :: !(Maybe TestInside)
    , testEven   :: !Int
    , testN      :: !N
    , testC      :: !ColorScheme
    , testPair   :: !(Int, Text)
    , testTriple :: !(Int, Text, Bool)
    , testE1     :: !(Either Integer String)
    , testE2     :: !(Either String Double)
    , testStatus :: !UserStatus
    , users      :: ![User]
    , susers     :: !(Set User)
    , husers     :: !(HashSet User)
    , intset     :: !IntSet
    , payloads   :: !(Map Text Int)
    , colours    :: !(Map Text Colour)
    }


testT :: TomlCodec Test
testT = Test
    <$> Toml.bool "testB" .= testB
    <*> Toml.int "testI" .= testI
    <*> Toml.double "testF" .= testF
    <*> Toml.text "testS" .= testS
    <*> Toml.arrayOf Toml._Text "testA" .= testA
    <*> Toml.arrayNonEmptyOf Toml._Text "testNE" .= testNE
    <*> Toml.nonEmpty (Toml.int "a") "testNET" .= testNET
    <*> Toml.dioptional (Toml.bool "testM") .= testM
    <*> Toml.table insideCodec "testX" .= testX
    <*> Toml.dioptional (Toml.table insideCodec "testY") .= testY
    <*> Toml.validateIf even Toml._Int "testEven" .= testEven
    <*> Toml.diwrap (Toml.text "testN") .= testN
    <*> Toml.enumBounded "testC" .= testC
    <*> Toml.table pairC "testPair" .= testPair
    <*> Toml.table tripleC "testTriple" .= testTriple
    <*> eitherT1 .= testE1
    <*> eitherT2 .= testE2
    <*> userStatusCodec .= testStatus
    <*> Toml.list userCodec "user" .= users
    <*> Toml.set userCodec "suser" .= susers
    <*> Toml.hashSet userCodec "huser" .= husers
    <*> Toml.arrayIntSet "intset" .= intset
    <*> Toml.map (Toml.text "name") (Toml.int "payload") "payloads" .= payloads
    <*> Toml.tableMap Toml._KeyText colourCodec "colours" .= colours
  where
    pairC :: TomlCodec (Int, Text)
    pairC = Toml.pair (Toml.int "pNum") (Toml.text "pName")

    tripleC :: TomlCodec (Int, Text, Bool)
    tripleC = Toml.triple (Toml.int "tNum") (Toml.text "tName") (Toml.bool "isFav")

    -- different keys for sum type
    eitherT1 :: TomlCodec (Either Integer String)
    eitherT1 = Toml.match (Toml._Left >>> Toml._Integer)  "either.Left"
           <|> Toml.match (Toml._Right >>> Toml._String) "either.Right"

    -- same key for sum type;
    -- doesn't work if you have something like `Either String String`,
    -- you should distinguish these cases by different keys like in `eitherT1` example
    eitherT2 :: TomlCodec (Either String Double)
    eitherT2 = ( Toml.match (Toml._Left >>> Toml._String)
             <!> Toml.match (Toml._Right >>> Toml._Double)
               ) "either"

data GenericPerson = GenericPerson
    { genericPersonName    :: !Text
    , genericPersonAddress :: !Address
    } deriving stock (Generic)

data Address = Address
    { addressStreet :: !Text
    , addressHouse  :: !Int
    } deriving stock (Generic)
      deriving HasCodec via (TomlTable Address)

testGeneric :: TomlCodec GenericPerson
testGeneric = stripTypeNameCodec

newtype MyByteString = MyByteString
    { unMyByteString :: ByteString
    } deriving HasCodec via ByteStringAsBytes

main :: IO ()
main = do
    TIO.putStrLn "=== Printing manually specified TOML ==="
    TIO.putStrLn $ pretty myToml

    TIO.putStrLn "=== Trying to print invalid TOML ==="
    content <- TIO.readFile "examples/invalid.toml"
    TIO.putStrLn $ case Toml.parse content of
        Left (TomlParseError e) -> e
        Right toml              -> pretty toml

    TIO.putStrLn "=== Testing bidirectional conversion ==="
    biFile <- TIO.readFile "examples/biTest.toml"
    TIO.putStrLn $ case Toml.decode testT biFile of
        Left msgs  -> Toml.prettyTomlDecodeErrors msgs
        Right test -> Toml.encode testT test

    TIO.putStrLn "=== Testing Deriving Via ==="
    genericFile <- TIO.readFile "examples/generic.toml"
    TIO.putStrLn $ case Toml.decode testGeneric genericFile of
        Left msg   -> Toml.prettyTomlDecodeErrors msg
        Right test -> Toml.encode testGeneric test

myToml :: TOML
myToml = mkToml $ do
    "a" =: Bool True
    "list" =: Array ["one", "two"]
    "time" =: Array [Day (fromGregorian 2018 3 29)]
    table "table.name.1" $ do
        "aInner" =: 1
        "listInner" =: Array [Bool True, Bool False]
        table "1" $ do
            "aInner11" =: 11
            "listInner11" =: Array [0, 1]
        table "2" $
            "Inner12" =: "12"
    table "table.name.2" $
        "Inner2" =: 42
