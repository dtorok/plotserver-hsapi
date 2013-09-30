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

plotCatSafe :: (String, String) -> String -> IO (Either String PlotData)
plotCatSafe auth dataset = safeResult <$> curlGetResponse_ url_ opts where
	url_ = actionUrl dataset "download"
	opts = defaultOpts auth

plotUpdateSafe :: (String, String) -> String -> PlotDataRow -> IO (Either String PlotData)
plotUpdateSafe auth dataset row = safeResult <$> curlGetResponse_ url_ opts where
	postData = show $ PlotData [row]
	url_ = actionUrl dataset "update"
	opts = defaultOpts auth ++ [
		CurlPost True, 
		CurlPostFields [postData]
		]

plotDeleteSafe :: (String, String) -> String -> IO (Either String PlotData)
plotDeleteSafe auth dataset = safeResult <$> curlGetResponse_ url_ opts where
	url_ = actionUrl dataset "delete"
	opts = defaultOpts auth

------ helpers ------

leftOrError :: Either String a -> a
leftOrError (Right a) = a
leftOrError (Left msg) = error msg

defaultOpts :: (String, String) -> [CurlOption]
defaultOpts (username, password) = [
		CurlUserPwd (username ++ ":" ++ password)
-- 		CurlHttpProxyTunnel True, 
-- 		CurlProxy "localhost:8888"
	]

actionUrl :: String -> String -> String
actionUrl dataset action = plotUrl dataset ++ "?" ++ action

safeResult :: CurlResponse_ [(String, String)] String -> Either String PlotData
safeResult response 
	| respStatus response == 200 = Right $ (read (respBody response) :: PlotData)
	| otherwise = Left err where
		err = "Error: " ++ show (respStatus response)
