module Plotserver.Api (url, cat, update, delete) where

import Network.Curl
import Control.Applicative

------ API ------

url :: String -> String
url dataset = "https://plot.prezi.com/" ++ dataset

cat :: (String, String) -> String -> IO (Either String String)
cat auth dataset = getResult <$> curlGetResponse_ url_ opts where
	url_ = actionUrl dataset "download"
	opts = defaultOpts auth

update :: (String, String) -> String -> String -> String -> IO (Either String String)
update auth dataset key values = getResult <$> curlGetResponse_ url_ opts where
	postData = key ++ "," ++ values
	url_ = actionUrl dataset "update"
	opts = defaultOpts auth ++ [
		CurlPost True, 
		CurlPostFields [postData]
		]

delete :: (String, String) -> String -> IO (Either String String)
delete auth dataset = getResult <$> curlGetResponse_ url_ opts where
	url_ = actionUrl dataset "delete"
	opts = defaultOpts auth

------ helpers ------

defaultOpts :: (String, String) -> [CurlOption]
defaultOpts (username, password) = [
		CurlUserPwd (username ++ ":" ++ password),
		CurlHttpProxyTunnel True, 
		CurlProxy "localhost:8888"
	]

actionUrl :: String -> String -> String
actionUrl dataset action = (url dataset) ++ "?" ++ action

getResult :: CurlResponse_ [(String, String)] body -> Either body String
getResult response 
	| respStatus response == 200 = Left $ respBody response
	| otherwise = Right err where
		err = "Error: " ++ (show $ respStatus response)