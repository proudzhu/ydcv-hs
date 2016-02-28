{-# LANGUAGE OverloadedStrings #-}

module YdResponse where

import Data.ByteString.Lazy.Char8 as BS
import Data.Aeson
import Data.Aeson.Types

testGood :: ByteString
testGood = "{\"translation\":[\"\229\165\189\"],\"basic\":{\"uk-speech\":\"http:\\/\\/fanyi.youdao.com\\/openapi.do?type=data&voice=true&version=1.2&key=659600698&keyfrom=YouDaoCV&q=good&vt=1\",\"us-phonetic\":\"\201\161\202\138d\",\"speech\":\"http:\\/\\/fanyi.youdao.com\\/openapi.do?type=data&voice=true&version=1.2&key=659600698&keyfrom=YouDaoCV&q=good&vt=1\",\"phonetic\":\"g\202\138d\",\"uk-phonetic\":\"g\202\138d\",\"us-speech\":\"http:\\/\\/fanyi.youdao.com\\/openapi.do?type=data&voice=true&version=1.2&key=659600698&keyfrom=YouDaoCV&q=good&vt=2\",\"explains\":[\"n. \229\165\189\229\164\132\239\188\155\229\150\132\232\161\140\239\188\155\230\133\183\230\133\168\231\154\132\232\161\140\228\184\186\",\"adj. \229\165\189\231\154\132\239\188\155\228\188\152\232\137\175\231\154\132\239\188\155\230\132\137\229\191\171\231\154\132\239\188\155\232\153\148\232\175\154\231\154\132\",\"adv. \229\165\189\",\"n. (Good)\228\186\186\229\144\141\239\188\155(\232\139\177)\229\143\164\229\190\183\239\188\155(\231\145\158\229\133\184)\230\136\136\229\190\183\"]},\"query\":\"good\",\"errorCode\":0,\"web\":[{\"value\":[\"\229\165\189\",\"\232\137\175\229\165\189\",\"\229\149\134\229\147\129\"],\"key\":\"Good\"},{\"value\":[\"\232\128\182\231\168\163\229\143\151\233\154\190\232\138\130\",\"\232\128\182\231\168\163\229\143\151\233\154\190\230\151\165\",\"\229\143\151\233\154\190\232\138\130\"],\"key\":\"Good Friday\"},{\"value\":[\"\236\156\132\237\130\164\235\176\177\234\179\188 \235\143\153\236\157\140\236\157\180\236\157\152\236\150\180 \235\172\184\236\132\156\",\"Good Time\",\"Good Time\"],\"key\":\"Good Time\"}]}"

testOptional :: ByteString
testOptional = "{\"translation\":[\"\228\189\160\229\165\189\"],\"basic\":{\"uk-speech\":\"http:\\/\\/fanyi.youdao.com\\/openapi.do?type=data&voice=true&version=1.2&key=659600698&keyfrom=YouDaoCV&q=nihao&vt=1\",\"speech\":\"http:\\/\\/fanyi.youdao.com\\/openapi.do?type=data&voice=true&version=1.2&key=659600698&keyfrom=YouDaoCV&q=nihao&vt=1\",\"us-speech\":\"http:\\/\\/fanyi.youdao.com\\/openapi.do?type=data&voice=true&version=1.2&key=659600698&keyfrom=YouDaoCV&q=nihao&vt=2\",\"explains\":[\"\228\189\160\229\165\189\239\188\136\228\184\173\230\150\135\230\139\188\233\159\179\239\188\137\"]},\"query\":\"nihao\",\"errorCode\":0,\"web\":[{\"value\":[\"\228\184\170\230\128\167\231\173\190\229\144\141\"],\"key\":\"nihao\"},{\"value\":[\"\229\156\176\229\140\186\233\152\191\230\160\185\229\187\183\228\185\176\229\174\182\",\"\229\156\176\229\140\186\233\152\191\230\160\185\229\187\183\232\191\155\229\143\163\229\149\134\"],\"key\":\"NIHAO SRL\"},{\"value\":[\"\233\147\129\232\190\190\229\176\188\229\143\183\230\178\137\230\178\161\232\174\176\"],\"key\":\"tieda nihao\"}]}"

testBasic :: ByteString
testBasic = "{\"uk-speech\":\"http:\\/\\/fanyi.youdao.com\\/openapi.do?type=data&voice=true&version=1.2&key=659600698&keyfrom=YouDaoCV&q=nihao&vt=1\",\"speech\":\"http:\\/\\/fanyi.youdao.com\\/openapi.do?type=data&voice=true&version=1.2&key=659600698&keyfrom=YouDaoCV&q=nihao&vt=1\",\"us-speech\":\"http:\\/\\/fanyi.youdao.com\\/openapi.do?type=data&voice=true&version=1.2&key=659600698&keyfrom=YouDaoCV&q=nihao&vt=2\",\"explains\":[\"\228\189\160\229\165\189\239\188\136\228\184\173\230\150\135\230\139\188\233\159\179\239\188\137\"]}"

testChinese :: ByteString
testChinese = "{\"translation\":[\"Well you didn't\"],\"query\":\"\229\165\189\228\189\160\230\178\161\",\"errorCode\":0}"

data Dict = Dict
    { translation :: [String]
    , basic       :: Maybe BasicDict
    , query       :: String
    , errorCode   :: Int
    , web         :: Maybe [WebDict]
    } deriving (Show)

data BasicDict = BasicDict
    { ukSpeech   :: Maybe String
    , usPhonetic :: Maybe String
    , speech     :: Maybe String
    , phonetic   :: Maybe String
    , ukPhonetic :: Maybe String
    , usSpeech   :: Maybe String
    , explains   :: [String]
    } deriving (Show)

data WebDict = WebDict
    { value :: [String]
    , key :: String
    } deriving (Show)

instance FromJSON Dict where
  parseJSON (Object v) =
    Dict <$> v .: "translation"
         <*> v .:? "basic"
         <*> v .: "query"
         <*> v .: "errorCode"
         <*> v .:? "web"

instance FromJSON BasicDict where
  parseJSON (Object v) =
    BasicDict <$> v .:? "uk-speech"
              <*> v .:? "uk-phonetic"
              <*> v .:? "speech"
              <*> v .:? "phonetic"
              <*> v .:? "uk-phonetic"
              <*> v .:? "us-speech"
              <*> v .: "explains"

instance FromJSON WebDict where
  parseJSON (Object v) =
    WebDict <$> v .: "value"
            <*> v .: "key"

