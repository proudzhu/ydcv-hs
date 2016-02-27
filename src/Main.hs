{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import Network.HTTP
import Data.Maybe (fromJust)
import Data.ByteString.Lazy.Char8 as BS
import Data.Aeson as DA
import YdResponse
import Text.PrettyPrint.ANSI.Leijen
import Formatter (renderDictLn)
import Options.Applicative
import ArgParse

api :: String
api = "YouDaoCV"

apiKey :: String
apiKey = "659600698"

apiVersion :: String
apiVersion = "1.2"

ydBaseUrl :: String
ydBaseUrl = "http://fanyi.youdao.com/openapi.do?keyfrom=" ++ api ++
            "&key=" ++ apiKey ++
            "&type=data&doctype=json&version=" ++ apiVersion

getWordUrl :: String -> String
getWordUrl word = ydBaseUrl ++ "&q=" ++ word

testUrl :: String
testUrl = "http://fanyi.youdao.com/openapi.do?keyfrom=YouDaoCV&key=659600698&type=data&doctype=json&version=1.2&q=good"

get :: String -> IO String
get url = simpleHTTP (getRequest url) >>= getResponseBody

getCode :: String -> IO ResponseCode
getCode url = simpleHTTP req >>= getResponseCode
    where req = getRequest url

decodeDict :: String -> Maybe Dict
decodeDict str = DA.decode (BS.pack str)

lookupWord :: String -> IO ()
lookupWord word = do
                res <- get (getWordUrl word)
                renderDictLn (fromJust (decodeDict res :: Maybe Dict))

lookupAllWords :: [String] -> IO ()
lookupAllWords = mapM_ lookupWord

main :: IO ()
main = do
     conf <- execParser (info argParse fullDesc)
     lookupAllWords (wordList conf)
