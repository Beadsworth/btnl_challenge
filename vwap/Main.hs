-- import VWAP.In (processCSVLine, emptyPreReport, PreReport)
-- import VWAP.Out (preReport2ReportJson)
-- import System.IO (hGetLine, stdin, hIsEOF)
-- -- import qualified Data.ByteString.Lazy.Char8 as BL
-- import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Lazy as BL
-- import Data.Csv
-- import qualified Data.Vector as V
-- import qualified Data.ByteString.Lazy.Char8 as B





-- -- Function to process stdin line by line and sum quantities
-- processStdin :: PreReport -> IO PreReport
-- processStdin preReport = do
--     eof <- hIsEOF stdin
--     if eof
--         then return preReport
--         else do
--             line <- BL.hGetContents
--             lines = lines
--             let newPreReport = processCSVLine preReport line
--             processStdin newPreReport


-- main :: IO ()
-- main = do
--     -- process entire CSV input, starting from empty PreReport
--     preReport <- processStdin emptyPreReport
--     -- convert PreReport into Report
--     let reportJSON = preReport2ReportJson preReport
--     -- print Report to stdout
--     B.putStrLn reportJSON



{-# LANGUAGE OverloadedStrings #-}


import qualified Data.Csv as Csv
import qualified Data.Csv.Streaming as CsvS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Foldable as F
import System.IO (stdin)
import Data.Int (Int64)
import Data.Text (Text)


-- define colun types
data Match = Match
    { maker :: !Text
    , taker :: !Text
    , symbol :: !Text
    , side :: !Text
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


-- Function to fold over records
totalsFold :: (Int64, Int64) -> Match -> (Int64, Int64)
totalsFold (totalPrice, totalQuantity) (Match _ _ _ _ price quantity) =
    (totalPrice + price, totalQuantity + quantity)


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
    let result = F.foldl' processRow (0, 0) csvStream
    
    -- Print the result
    print result


-- Helper function to process each row
processRow :: (Int64, Int64) -> Match -> (Int64, Int64)
processRow acc match = totalsFold acc match
