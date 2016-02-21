{-# LANGUAGE OverloadedStrings #-}

module YdResponse where

import Data.ByteString.Lazy.Char8 as BS
import Data.Aeson
import Data.Aeson.Types

data Dict = Dict
    { translation :: [String]
    , basic       :: BasicDict
    , query       :: String
    , errorCode   :: Int
    , web         :: [WebDict]
    } deriving (Show)

data BasicDict = BasicDict
    { ukSpeech   :: String
    , usPhonetic :: String
    , speech      :: String
    , phonetic    :: String
    , ukPhonetic :: String
    , usSpeech   :: String
    , explains    :: [String]
    } deriving (Show)

data WebDict = WebDict
    { value :: [String]
    , key :: String
    } deriving (Show)

instance FromJSON Dict where
  parseJSON (Object v) =
    Dict <$> v .: "translation"
         <*> v .: "basic"
         <*> v .: "query"
         <*> v .: "errorCode"
         <*> v .: "web"

instance FromJSON BasicDict where
  parseJSON (Object v) =
    BasicDict <$> v .: "uk-speech"
              <*> v .: "uk-phonetic"
              <*> v .: "speech"
              <*> v .: "phonetic"
              <*> v .: "uk-phonetic"
              <*> v .: "us-speech"
              <*> v .: "explains"

instance FromJSON WebDict where
  parseJSON (Object v) =
    WebDict <$> v .: "value"
            <*> v .: "key"

