-- Module For Parsing input CSV

module VWAP.In  
( Side (..)
, Match (..)
, PreReport (..)
, parseCSVLine
, emptyPreReport
, updatePreReport
) where


import Data.Maybe (isJust, fromJust, catMaybes)
import Data.List.Split (splitOn)
import Data.Int (Int64)
import Data.Word (Word32)
import qualified Data.Map as Map
import qualified Data.Map.Strict as Map.Strict


-- Word32 is actually uint32
type UInt64 = Word32
type Symbol = String


data Side = Ask | Bid
    deriving (Eq, Ord, Show, Read)


-- input CSV lines
data Match = Match
    { maker :: String
    , taker :: String
    , symbol :: Symbol
    , side :: Side
    , price :: Int64
    , quantity :: UInt64
    } deriving (Show)


-- sums of (price * quantity) & volume
-- vwap is just cumPQ / volume
data PreReportValues = PreReportValues
    { cumPQ :: Int
    , cumQuant :: Int
    } deriving (Show)


data ReportValues = ReportValues
    { vwap :: Int
    , volume :: Int
    } deriving (Show) 


-- a map of Symbol-to-PreReportValues
type PreReport = Map.Map Symbol PreReportValues
-- type for final report
type Report = Map.Map Symbol ReportValues

emptyPreReport :: PreReport
emptyPreReport = Map.empty :: PreReport


-- simple CSV line parser
-- probably could use cassava library
--  if more advanced CSV parsing is needed
parseCSVLine :: String -> Match
parseCSVLine line = match
    where
        [col0, col1, col2, col3, col4, col5] = splitOn "," line
        match = Match   { maker = col0
                        , taker = col1
                        , symbol = col2
                        , side = read col3
                        , price = read col4
                        , quantity = read col5
                        }


-- calculate p*q for one match 
pq :: Match -> Int
pq match = result
    where
        intPrice = fromIntegral $ price match
        intQuantity = fromIntegral $ quantity match
        result = intPrice * intQuantity


-- add two PreReport values together
addPreReportValues :: PreReportValues -> PreReportValues -> PreReportValues
addPreReportValues preReportValues1 preReportValues2 = result
    where
        addedCumPQ = (cumPQ preReportValues1) + (cumPQ preReportValues2)
        addedCumQuant = (cumQuant preReportValues1) + (cumQuant preReportValues2)
        result = PreReportValues {cumPQ = addedCumPQ, cumQuant = addedCumQuant}
        

updatePreReport :: PreReport -> Match -> PreReport
updatePreReport preReport match = result
    where
        symbol' = symbol match
        quantity' = fromIntegral $ quantity match
        nextPreReportValues = PreReportValues {cumPQ = (pq match), cumQuant = quantity'}
        -- use strict function since stdin is parsed
        -- line-by-line anyway
        result = Map.Strict.insertWith addPreReportValues symbol' nextPreReportValues preReport
