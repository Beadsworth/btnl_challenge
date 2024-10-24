-- Module for printing out JSON results
{-# LANGUAGE OverloadedStrings #-}


module VWAP.Out 
( sumsMap2ReportJSON,
  sumsMap2Report
) where


import Data.Int (Int64)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson (ToJSON, toJSON, object, (.=), encode, toEncoding)

import VWAP.In (SumsMap, Sums (..), Symbol)


-- final values reported for each symbol
data ReportValues = ReportValues
    { vwap :: !Double
    , volume :: !Int64
    } deriving (Show)


-- round to one decimal place
roundToOneDecimal :: Double -> Double
roundToOneDecimal x = fromIntegral (round (x * 10)) / 10


-- define ReportValues output JSON
instance ToJSON ReportValues where
    toJSON (ReportValues vwap volume) =
        object 
            [ "vwap" .= vwap
            , "volume" .= volume
            ]


-- a map of Symbol-to-ReportValues
type Report = Map.Map Symbol ReportValues


safeDivide :: Double -> Double -> Double
safeDivide n 0 = 0 :: Double
safeDivide n d = n / d :: Double


-- volume = quantSum
-- VWAP = weightedSum / volume
convertValues :: Sums -> ReportValues
convertValues sums = reportValues
    where
        -- volume should never be zero, skipping validation...
        (Sums weightedSum volume) = sums
        doubleWeightedSum = fromIntegral weightedSum :: Double
        doubleVolume = fromIntegral volume :: Double
        rawVWAP = doubleWeightedSum `safeDivide` doubleVolume
        vwap = roundToOneDecimal rawVWAP
        reportValues = ReportValues {vwap = vwap, volume = volume}


-- convert SumsMap Map to Report
sumsMap2Report :: SumsMap -> Report
sumsMap2Report sumsMap = result
    where
        result = Map.map convertValues sumsMap


-- function to convert a Map to JSON
report2Json :: Report -> B.ByteString
report2Json report = encode $ toJSON report


-- convenience function, just export this
sumsMap2ReportJSON :: SumsMap -> B.ByteString
sumsMap2ReportJSON = report2Json . sumsMap2Report
