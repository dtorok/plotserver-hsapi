module Plotserver.Types where

import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Traversable

-- TODO what if there isn't any password?
data PlotConfig = PlotConfig {
	server :: String,
	username :: String,
	password :: String
} deriving (Eq, Show)

data PlotData = PlotData [PlotDataRow]
type PlotDataRow = (String, [Int])

instance Show PlotData where
	show (PlotData rows) = intercalate "\n" $ map showRow rows where
				showRow :: (String, [Int]) -> String
				showRow (key, values) = key ++ ", " ++ showValues values

				showValues :: [Int] -> String
				showValues values = intercalate ", " (map show values)

instance Read PlotData where
	readsPrec _ s = case dataRows of
							Just dataRows' -> [(PlotData dataRows', "")]
							Nothing -> []
	 where
		rows = filter (not.null) $ splitOn "\n" s
		dataRows = traverse createDataRow rows :: Maybe [PlotDataRow]
		createDataRow row = createDataTuple $ splitOn "," row :: Maybe PlotDataRow

		createDataTuple [] = Nothing
		createDataTuple (key:sValues) = Just (key, map read sValues) :: Maybe PlotDataRow -- just for the readibility
