-- Module For Parsing input CSV


module VWAP.In  
( PreReport (..)
, PreReportValues (..)
, Symbol
, processCSVLine
, emptyPreReport
) where


import Data.List.Split (splitOn)
import Data.Int (Int64)
import Data.Word (Word32)
import qualified Data.Map as Map
import qualified Data.Map.Strict as Map.Strict
import qualified Data.ByteString.Lazy as BL
import Data.Text    (Text)
import Data.Csv
import qualified Data.Vector as V
import Control.Monad (mzero)



-- Word32 is actually uint32
type UInt32 = Word32
type Symbol = Text


data Side = Ask | Bid
    deriving (Eq, Ord, Show, Read)


-- input CSV lines
data Match = Match
    { maker :: !Text
    , taker :: !Text
    , symbol :: !Symbol
    , side :: !Text
    , price :: !Int64
    , quantity :: !UInt32
    } deriving (Show)


-- Define how to get a Match from a record (CSV row)
instance FromRecord Match where
  parseRecord record =
    Match
      <$> record .! 0
      <*> record .! 1
      <*> record .! 2
      <*> record .! 3
      <*> record .! 4
      <*> record .! 5


-- sums of (price * quantity) & volume
-- vwap is just cumPQ / volume
data PreReportValues = PreReportValues
    { cumPQ :: Int
    , cumQuant :: Int
    } deriving (Show)


-- a map of Symbol-to-PreReportValues
type PreReport = Map.Map Symbol PreReportValues


-- return empty Map of correct type
emptyPreReport :: PreReport
emptyPreReport = Map.empty :: PreReport


-- simple CSV line parser
-- probably could use cassava library
--  if more advanced CSV parsing is needed
parseCSVLine :: BL.ByteString -> Match
parseCSVLine line = match
    where
        match = 
            case decode NoHeader line of
                Left err -> error err
                Right v  ->
                    if V.null v
                        then error "No data found"
                        else (V.head v)

    -- where
    --     match = decode NoHeader line
        -- -- char 44 is comma
        -- [col0, col1, col2, col3, col4, col5] = splitOn "," line
        -- match = Match   { maker = col0
        --                 , taker = col1
        --                 , symbol = col2
        --                 , side = read col3
        --                 , price = read col4
        --                 , quantity = read col5
        --                 }


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
        

-- update a PreReport map with one Match reading
updatePreReport :: PreReport -> Match -> PreReport
updatePreReport preReport match = result
    where
        symbol' = symbol match
        quantity' = fromIntegral $ quantity match
        nextPreReportValues = PreReportValues {cumPQ = (pq match), cumQuant = quantity'}
        -- if symbol is absent, add the new PreReportValue
        --  if symbol already exists, add new PreRportValues to existing values
        --  use strict function since stdin is parsed line-by-line anyway
        result = Map.Strict.insertWith addPreReportValues symbol' nextPreReportValues preReport


-- process a single line from the CSV
processCSVLine :: PreReport -> BL.ByteString -> PreReport
processCSVLine preReport line = updatedPreReport
    where
        -- parse a single CSV line
        match = parseCSVLine line
        -- update cumulative preReport map with the match
        updatedPreReport = updatePreReport preReport match
