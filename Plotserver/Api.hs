module Plotserver.Api (
	plotUrl, plotCat, plotUpdate, plotDelete
	) where

import Network.Curl
import Control.Applicative ((<$>))

import Plotserver.Types

------ API ------

plotUrl :: String -> String
plotUrl dataset = "https://plot.prezi.com/" ++ dataset

plotCat :: (String, String) -> String -> IO (Either String PlotData)
plotCat auth dataset = response2either <$> curlGetResponse_ url_ opts where
	url_ = actionUrl dataset "download"
	opts = defaultOpts auth

plotUpdate :: (String, String) -> String -> PlotDataRow -> IO (Either String PlotData)
plotUpdate auth dataset row = response2either <$> curlGetResponse_ url_ opts where
	postData = show $ PlotData [row]
	url_ = actionUrl dataset "update"
	opts = defaultOpts auth ++ [
		CurlPost True,
		CurlPostFields [postData]
		]

plotDelete :: (String, String) -> String -> IO (Either String PlotData)
plotDelete auth dataset = response2either <$> curlGetResponse_ url_ opts where
	url_ = actionUrl dataset "delete"
	opts = defaultOpts auth

------ helpers ------

defaultOpts :: (String, String) -> [CurlOption]
defaultOpts (uname, pwd) = [
		CurlUserPwd (uname ++ ":" ++ pwd)
	]

actionUrl :: String -> String -> String
actionUrl dataset action = plotUrl dataset ++ "?" ++ action

response2either :: CurlResponse_ [(String, String)] String -> Either String PlotData
response2either response 
	| respStatus response == 200 = Right $ (read (respBody response) :: PlotData)
	| otherwise = Left err where
		err = "Error: " ++ show (respStatus response)
