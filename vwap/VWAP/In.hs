-- Module For Parsing input CSV
{-# LANGUAGE OverloadedStrings #-}


module VWAP.In
( Match (..)
, Sums (..)
, SumsMap (..)
, Symbol
, updateSumsMap
, emptySumsMap
) where


import Data.Text (Text)
import Data.Int (Int64)
import qualified Data.Map as Map
import qualified Data.Csv as Csv
import qualified Data.Csv.Streaming as CsvS
import Data.Csv (Parser, Field, FromField)


type Symbol = Text
data Side = Bid | Ask
    deriving (Show, Read)


instance FromField Side where
    parseField s
        | s == "Bid"  = pure Bid
        | s == "Ask"  = pure Ask
        | otherwise = fail $ "bad parsing of side"


-- define colun types
data Match = Match
    { maker :: !Text
    , taker :: !Text
    , symbol :: !Symbol
    , side :: !Side
    , price :: !Int64
    , quantity :: !Int64
    } deriving (Show)


-- parse columns into records
instance Csv.FromRecord Match where
    parseRecord v = Match 
        <$> v Csv..! 0 
        <*> v Csv..! 1 
        <*> v Csv..! 2 
        <*> v Csv..! 3 
        <*> v Csv..! 4 
        <*> v Csv..! 5 


-- sums of (price * quantity) & volume
-- vwap is just cumPQ / volume
data Sums = Sums
    { weightedSum :: !Int64
    , quantSum :: !Int64
    } deriving (Show)


-- a Map of Symbol: Sums
type SumsMap = Map.Map Symbol Sums


-- return empty Map of correct type
emptySumsMap :: SumsMap
emptySumsMap = Map.empty :: SumsMap


-- calculate p*q for one match 
pq :: Match -> Int64
pq match = (price match) * (quantity match)


-- add two Sums together
combineSums :: Sums -> Sums -> Sums
combineSums sums1 sums2 = 
    Sums
        {
           weightedSum = (ws1 + ws2),
           quantSum = (qs1 + qs2)
        }
    where
        (Sums ws1 qs1) = sums1
        (Sums ws2 qs2) = sums2
        

-- update a SumsMap with one Match reading
updateSumsMap :: SumsMap -> Match -> SumsMap
updateSumsMap sumsMap match = result
    where
        (Match _ _ symbol _ _ quantity) = match
        nextSums = Sums {weightedSum = (pq match), quantSum = quantity}
        -- if symbol is absent, add the key: value to SumsMap
        --  if symbol already exists, add new Sums to existing Sums
        result = Map.insertWith combineSums symbol nextSums sumsMap
