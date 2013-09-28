{-# LANGUAGE OverloadedStrings #-}

module Plotserver.Types where

import Data.List
import Data.List.Split

-- TODO what if there isn't any password?
data Config = Config {
	server :: String,
	username :: String,
	password :: String
}

data PlotData = PlotData [PlotDataRow]
type PlotDataRow = (String, [Int])

instance Show PlotData where
	show (PlotData rows) = intercalate "\n" $ map showRow rows where
				showRow :: (String, [Int]) -> String
				showRow (key, values) = key ++ ", " ++ showValues values

				showValues :: [Int] -> String
				showValues values = intercalate ", " (map show values)

instance Read PlotData where
	readsPrec _ s = [(PlotData dataRows, "")] where
		rows = splitOn "\n" s
		dataRows = map createDataRow rows
		createDataRow row = createDataTuple $ splitOn "," row
		createDataTuple (key:sValues) = (key, map read sValues) :: (String, [Int]) -- just for the readibility
