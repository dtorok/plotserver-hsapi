{-# LANGUAGE NamedFieldPuns  #-}

module Plotserver.Api (
	plotUrl, plotCat, plotUpdate, plotDelete
	) where

import Network.Curl
import Control.Applicative ((<$>))

import Plotserver.Types

------ API ------

plotUrl :: PlotConfig -> String -> String
plotUrl (PlotConfig {server}) dataset = server ++ "/" ++ dataset

plotCat :: PlotConfig -> String -> IO (Either String PlotData)
plotCat config dataset = response2either <$> curlGetResponse_ url_ opts where
	url_ = actionUrl config dataset "download"
	opts = defaultOpts config

plotUpdate :: PlotConfig -> String -> PlotDataRow -> IO (Either String PlotData)
plotUpdate config dataset row = response2either <$> curlGetResponse_ url_ opts where
	postData = show $ PlotData [row]
	url_ = actionUrl config dataset "update"
	opts = defaultOpts config ++ [
		CurlPost True,
		CurlPostFields [postData]
		]

plotDelete :: PlotConfig -> String -> IO (Either String PlotData)
plotDelete config dataset = response2either <$> curlGetResponse_ url_ opts where
	url_ = actionUrl config dataset "delete"
	opts = defaultOpts config

------ helpers ------

defaultOpts :: PlotConfig -> [CurlOption]
defaultOpts PlotConfig {username, password} = [
		CurlUserPwd (username ++ ":" ++ password)
	]

actionUrl :: PlotConfig -> String -> String -> String
actionUrl config dataset action = plotUrl config dataset ++ "?" ++ action

response2either :: CurlResponse_ [(String, String)] String -> Either String PlotData
response2either response 
	| respStatus response == 200 = Right $ (read (respBody response) :: PlotData)
	| otherwise = Left err where
		err = "Error: " ++ show (respStatus response)
