{-# OPTIONS -Wno-unused-top-binds #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main (main) where

import Control.Applicative ((<|>))
import Control.Arrow ((>>>))
import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (fromGregorian)
import GHC.Generics (Generic)

import Toml (TomlCodec, TomlParseError (..), pretty, (.=), (<!>))
import Toml.Type (TOML (..), Value (..))
import Toml.Type.Edsl (mkToml, table, (=:))

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
    deriving stock (Show, Enum, Bounded)

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
userPassCodec = (,)
    <$> Toml.text "username" .= fst
    <*> Toml.text "password" .= snd

userStatusCodec :: TomlCodec UserStatus
userStatusCodec =
    Toml.dimatch matchRegistered (uncurry Registered) (Toml.table userPassCodec "testStatus")
    <|> Toml.dimatch matchAnonymous Anonymous (Toml.text "testStatus")

data Version =
      SimpleVersion !Text
    | GitVersion !Text !(Maybe Text)
    | PathVersion !Text !(Maybe Text)

matchSimpleVersion :: Version -> Maybe Text
matchSimpleVersion (SimpleVersion v) = Just v
matchSimpleVersion _                 = Nothing

matchGitVersion :: Version -> Maybe (Text, Maybe Text)
matchGitVersion (GitVersion path version) = Just (path, version)
matchGitVersion _                         = Nothing

matchPathVersion :: Version -> Maybe (Text, Maybe Text)
matchPathVersion (PathVersion path version) = Just (path, version)
matchPathVersion _                          = Nothing

gitVersionCodec :: TomlCodec (Text, Maybe Text)
gitVersionCodec = (,)
    <$> Toml.text "git" .= fst
    <*> Toml.dioptional (Toml.text "version") .= snd

pathVersionCodec :: TomlCodec (Text, Maybe Text)
pathVersionCodec = (,)
    <$> Toml.text "path" .= fst
    <*> Toml.dioptional (Toml.text "version") .= snd

versionCodec :: Toml.Key -> TomlCodec Version
versionCodec key =
    Toml.dimatch matchSimpleVersion SimpleVersion (Toml.text key)
    <|> Toml.dimatch matchGitVersion (uncurry GitVersion) (Toml.table gitVersionCodec key)
    <|> Toml.dimatch matchPathVersion (uncurry PathVersion) (Toml.table pathVersionCodec key)

data Test = Test
    { testB        :: !Bool
    , testI        :: !Int
    , testF        :: !Double
    , testS        :: !Text
    , testA        :: ![Text]
    , testM        :: !(Maybe Bool)
    , testX        :: !TestInside
    , testY        :: !(Maybe TestInside)
    , testN        :: !N
    , testC        :: !ColorScheme
    , testE1       :: !(Either Integer String)
    , testE2       :: !(Either String Double)
    , testStatus   :: !UserStatus
    , users        :: ![User]
    , susers       :: !(Set User)
    , husers       :: !(HashSet User)
    , payloads     :: !(Map Text Int)
    , dependencies :: !(Map Text Version)
    }


testT :: TomlCodec Test
testT = Test
    <$> Toml.bool "testB" .= testB
    <*> Toml.int "testI" .= testI
    <*> Toml.double "testF" .= testF
    <*> Toml.text "testS" .= testS
    <*> Toml.arrayOf Toml._Text "testA" .= testA
    <*> Toml.dioptional (Toml.bool "testM") .= testM
    <*> Toml.table insideCodec "testX" .= testX
    <*> Toml.dioptional (Toml.table insideCodec "testY") .= testY
    <*> Toml.diwrap (Toml.text "testN") .= testN
    <*> Toml.enumBounded "testC" .= testC
    <*> eitherT1 .= testE1
    <*> eitherT2 .= testE2
    <*> userStatusCodec .= testStatus
    <*> Toml.list userCodec "user" .= users
    <*> Toml.set userCodec "suser" .= susers
    <*> Toml.hashSet userCodec "huser" .= husers
    <*> Toml.map (Toml.text "name") (Toml.int "payload") "payloads" .= payloads
    <*> Toml.dynamicMapCodec Toml._KeyText versionCodec "dependencies" .= dependencies
  where
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
        Left msg   -> Toml.prettyTomlDecodeError msg
        Right test -> Toml.encode testT test

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
