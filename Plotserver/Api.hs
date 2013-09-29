{-# LANGUAGE OverloadedStrings #-}

module Plotserver.Api (
	plotUrl, 
	plotCat, plotUpdate, plotDelete,
	plotCatSafe, plotUpdateSafe, plotDeleteSafe
	) where

import Network.Curl
import Control.Applicative ((<$>))

import Plotserver.Types

------ API ------

plotUrl :: String -> String
plotUrl dataset = "https://plot.prezi.com/" ++ dataset

plotCat :: (String, String) -> String -> IO PlotData
plotCat auth dataset = leftOrError <$> plotCatSafe auth dataset

plotUpdate :: (String, String) -> String -> PlotDataRow -> IO PlotData
plotUpdate auth dataset row = leftOrError <$> plotUpdateSafe auth dataset row

plotDelete :: (String, String) -> String -> IO PlotData
plotDelete auth dataset = leftOrError <$> plotDeleteSafe auth dataset

------ SAFE API ------

plotCatSafe :: (String, String) -> String -> IO (Either PlotData String)
plotCatSafe auth dataset = getResult <$> curlGetResponse_ url_ opts where
	url_ = actionUrl dataset "download"
	opts = defaultOpts auth

plotUpdateSafe :: (String, String) -> String -> PlotDataRow -> IO (Either PlotData String)
plotUpdateSafe auth dataset row = getResult <$> curlGetResponse_ url_ opts where
	postData = show $ PlotData [row]
	url_ = actionUrl dataset "update"
	opts = defaultOpts auth ++ [
		CurlPost True, 
		CurlPostFields [postData]
		]

plotDeleteSafe :: (String, String) -> String -> IO (Either PlotData String)
plotDeleteSafe auth dataset = getResult <$> curlGetResponse_ url_ opts where
	url_ = actionUrl dataset "delete"
	opts = defaultOpts auth

------ helpers ------

leftOrError :: Either a String -> a
leftOrError (Left a) = a
leftOrError (Right msg) = error msg

defaultOpts :: (String, String) -> [CurlOption]
defaultOpts (username, password) = [
		CurlUserPwd (username ++ ":" ++ password),
		CurlHttpProxyTunnel True, 
		CurlProxy "localhost:8888"
	]

actionUrl :: String -> String -> String
actionUrl dataset action = plotUrl dataset ++ "?" ++ action

getResult :: CurlResponse_ [(String, String)] String -> Either PlotData String
getResult response 
	| respStatus response == 200 = Left $ (read (respBody response) :: PlotData)
	| otherwise = Right err where
		err = "Error: " ++ show (respStatus response)
