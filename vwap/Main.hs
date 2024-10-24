{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}


import qualified Data.Csv as Csv
import qualified Data.Csv.Streaming as CsvS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Foldable as F
import System.IO (stdin, hIsEOF)
import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar, forkIO)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (SomeException, AsyncException(UserInterrupt), catch, throwIO)
import Control.Monad (forever)
import qualified Web.Scotty as Scotty


import VWAP.In (Match, SumsMap, updateSumsMap, emptySumsMap)
import VWAP.Out (sumsMap2Report)


main :: IO ()
main = do

    -- start MVar with empty sumsMap
    sumsMapVar <- newMVar emptySumsMap

    -- start the scotty server
    Scotty.scotty 3000 $ do
        
        Scotty.post "/matches" $ do
            -- Read and decode the request body as UTF-8
            b <- Scotty.body

            let input = b
    
            -- parse the CSV using streaming
            -- if incoming CSV is large, this 
            -- will hopefully prevent large memory consumption
            let csvStream = CsvS.decode Csv.NoHeader input :: CsvS.Records Match

            -- fold over the stream to accumulate totals
            -- strict foldl prevents too much thunking
            let sumsMap = F.foldl' updateSumsMap emptySumsMap csvStream

            -- convert SumsMap into Report
            let report = sumsMap2Report sumsMap

            -- set response
            Scotty.json report


    where
        handleInterrupt :: SomeException -> IO ()
        handleInterrupt _ = return ()
