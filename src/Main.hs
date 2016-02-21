{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP
import Data.Maybe (fromJust)
import Data.ByteString.Lazy.Char8 as BS
import Data.Aeson as DA
import YdResponse
import Text.PrettyPrint.ANSI.Leijen
import Formatter (renderDictLn)

url :: String
url = "http://fanyi.youdao.com/openapi.do?keyfrom=YouDaoCV&key=659600698&type=data&doctype=json&version=1.2&q=good"

get :: String -> IO String
get url = simpleHTTP (getRequest url) >>= getResponseBody

getCode :: String -> IO ResponseCode
getCode url = simpleHTTP req >>= getResponseCode
    where req = getRequest url

decodeDict :: String -> Maybe Dict
decodeDict str = DA.decode (BS.pack str)

main :: IO ()
main = do
  res <- get url
  renderDictLn (fromJust (decodeDict res :: Maybe Dict))
