-- Module For Parsing input CSV
{-# LANGUAGE DeriveGeneric #-}


module VWAP.Out 
( Report (..)
, preReport2Json
) where


import GHC.Generics (Generic)
import Data.Aeson (ToJSON, toJSON, encode)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as B
import VWAP.In (PreReport, PreReportValues (..), Symbol)
import Text.Printf (printf)



data ReportValues = ReportValues
    { vwap :: Double
    , volume :: Int
    } deriving (Show, Generic)

-- set vwap precision
instance ToJSON ReportValues
    where
        toJSON (ReportValues vwap volume) =
            object 
                [ "vwap" .= (printf "%.1f" vwap :: String)
                , "volume" .= volume
                ]


-- a map of Symbol-to-ReportValues
type Report = Map.Map Symbol ReportValues


-- volume = cumQuant
-- VWAP = cumPQ / volume
convertValues :: PreReportValues -> ReportValues
convertValues preReportValues = reportValues
    where
        volume = cumQuant preReportValues
        doubleCumPQ = fromIntegral (cumPQ preReportValues) :: Double
        rawVWAP = doubleCumPQ / fromIntegral volume
        vwap = read (printf "%.1f" rawVWAP) :: Double
        reportValues = ReportValues {vwap = vwap, volume = volume}


-- convert PreReport Map to Report
preReport2Report :: PreReport -> Report
preReport2Report preReport = result
    where
        result = Map.map convertValues preReport



-- Function to convert a Map to JSON
report2Json :: Report -> B.ByteString
report2Json report = encode $ toJSON report



preReport2Json :: PreReport -> B.ByteString
preReport2Json = report2Json . preReport2Report
