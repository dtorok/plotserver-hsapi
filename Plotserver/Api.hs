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
plotCat config dataset = parseResponse <$> curlGetResponse_ url_ opts where
	url_ = actionUrl config dataset "download"
	opts = defaultOpts config
	-- TODO write real Maybe!
	parseResponse = response2either $ Just . (read :: (String -> PlotData))

plotUpdate :: PlotConfig -> String -> PlotDataRow -> IO (Either String PlotData)
plotUpdate config dataset row = parseResponse <$> curlGetResponse_ url_ opts where
	postData = show $ PlotData [row]
	url_ = actionUrl config dataset "update"
	opts = defaultOpts config ++ [
		CurlPost True,
		CurlPostFields [postData]
		]
	parseResponse = response2either $ matchString2MaybePlotdata "File written."

plotDelete :: PlotConfig -> String -> IO (Either String PlotData)
plotDelete config dataset = parseResponse <$> curlGetResponse_ url_ opts where
	url_ = actionUrl config dataset "delete"
	opts = defaultOpts config
	parseResponse = response2either $ matchString2MaybePlotdata "File deleted."

------ helpers ------

matchString2MaybePlotdata :: String -> String -> Maybe PlotData
matchString2MaybePlotdata expected real = if real == expected
									 then Just $ PlotData []
									 else Nothing

defaultOpts :: PlotConfig -> [CurlOption]
defaultOpts PlotConfig {username, password} = [
		CurlUserPwd (username ++ ":" ++ password),
		CurlFollowLocation True
	]

actionUrl :: PlotConfig -> String -> String -> String
actionUrl config dataset action = plotUrl config dataset ++ "?" ++ action

response2either :: (String -> Maybe PlotData) -> CurlResponse_ [(String, String)] String -> Either String PlotData
response2either parser response
	| respStatus response == 200 = case parser (respBody response) of
										Just res -> Right res
										Nothing  -> Left $ "Parse error: " ++ (respBody response)
	| otherwise = Left err where
		err = "Curl error: " ++ show (respStatus response) ++ " " ++ (respStatusLine response)
