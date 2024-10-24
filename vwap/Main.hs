{-# LANGUAGE OverloadedStrings #-}


import qualified Data.Csv as Csv
import qualified Data.Csv.Streaming as CsvS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Foldable as F
import System.IO (stdin)
import Control.Concurrent (newMVar, modifyMVar_, readMVar)
import Control.Monad.IO.Class (liftIO)


import VWAP.In (Match, updateSumsMap, emptySumsMap)
import VWAP.Out (sumsMap2ReportJSON)


main :: IO ()
main = do
    -- start MVar with empty sumsMap
    sumsMapVar <- newMVar emptySumsMap

    -- Read CSV data from stdin as lazy ByteString
    input <- BSL8.hGetContents stdin
    
    -- parse the CSV using streaming
    -- if incoming CSV is large, this 
    -- will hopefully prevent large memory consumption
    let csvStream = CsvS.decode Csv.NoHeader input :: CsvS.Records Match

    -- fold over the stream to accumulate totals
    -- strict foldl prevents too much thunking
    modifyMVar_ sumsMapVar $ \sumsMap ->
        return $ F.foldl' updateSumsMap sumsMap csvStream

    -- Grab the latest sumsMap
    sumsMap <- liftIO $ readMVar sumsMapVar

    -- convert SumsMap into Report
    let reportJSON = sumsMap2ReportJSON sumsMap
    
    -- print Report to stdout
    BSL8.putStrLn reportJSON
