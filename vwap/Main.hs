{-# LANGUAGE OverloadedStrings #-}




import qualified Data.Csv as Csv
import qualified Data.Csv.Streaming as CsvS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Foldable as F
import System.IO (stdin)

import VWAP.In (Match (..), updateSumsMap, emptySumsMap, SumsMap)
-- import VWAP.Out (preReport2ReportJson)


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
    let result = F.foldl' updateSumsMap emptySumsMap csvStream
    
    -- Print the result
    print result
