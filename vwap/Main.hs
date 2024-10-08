{-# LANGUAGE OverloadedStrings #-}


import qualified Data.Csv as Csv
import qualified Data.Csv.Streaming as CsvS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Foldable as F
import System.IO (stdin)

import VWAP.In (Match, updateSumsMap, emptySumsMap)
import VWAP.Out (sumsMap2ReportJSON)


main :: IO ()
main = do
    -- Read CSV data from stdin as lazy ByteString
    input <- BSL.hGetContents stdin
    
    -- parse the CSV using streaming
    -- if incoming CSV is large, this 
    -- will hopefully prevent large memory consumption
    let csvStream = CsvS.decode Csv.NoHeader input :: CsvS.Records Match

    -- fold over the stream to accumulate totals
    -- strict foldl prevents too much thunking
    let sumsMap = F.foldl' updateSumsMap emptySumsMap csvStream

    -- convert SumsMap into Report
    let reportJSON = sumsMap2ReportJSON sumsMap
    
    -- print Report to stdout
    BSL8.putStrLn reportJSON
