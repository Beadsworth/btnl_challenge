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
import VWAP.Out (sumsMap2ReportJSON)


-- Function to read from stdin and update the sumsMap
readFromStdin :: MVar SumsMap -> IO ()
readFromStdin sumsMapVar = forever $ do
    eof <- hIsEOF stdin
    if eof
        then throwIO UserInterrupt  -- This will break out of the forever loop
        else do
            line <- BS8.hGetLine stdin

            -- convert to lazy ByteString
            let input = BSL8.fromStrict line
            
            -- parse the CSV using streaming
            -- if incoming CSV is large, this 
            -- will hopefully prevent large memory consumption
            let csvStream = CsvS.decode Csv.NoHeader input :: CsvS.Records Match

            -- fold over the stream to accumulate totals
            -- strict foldl prevents too much thunking
            modifyMVar_ sumsMapVar $ \sumsMap ->
                return $! F.foldl' updateSumsMap sumsMap csvStream


main :: IO ()
main = do

    -- start MVar with empty sumsMap
    sumsMapVar <- newMVar emptySumsMap

    -- spawn the stdin reader in a separate thread using forkIO
    _ <- forkIO $ (readFromStdin sumsMapVar) `catch` handleInterrupt

    -- start the scotty server
    Scotty.scotty 3000 $ do
        Scotty.get "/summary" $ do

            -- Grab the latest sumsMap
            sumsMap <- liftIO $ readMVar sumsMapVar

            -- convert SumsMap into Report
            let reportJSON = sumsMap2ReportJSON sumsMap

            -- set header
            Scotty.setHeader "Content-Type" "application/json"

            -- set response
            Scotty.raw reportJSON

    where
        handleInterrupt :: SomeException -> IO ()
        handleInterrupt _ = return ()
