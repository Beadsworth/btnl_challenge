{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}


import qualified Data.Csv as Csv
import qualified Data.Csv.Streaming as CsvS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Foldable as F
import Data.Text (Text)
import System.IO (stdin, hIsEOF)
import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar, forkIO)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (SomeException, AsyncException(UserInterrupt), catch, throwIO)
import Control.Monad (forever)
import qualified Web.Scotty as Scotty
import qualified Network.Wai as Wai
import qualified Network.WebSockets as WS
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.Wai.Handler.Warp as Warp


import VWAP.In (Match, SumsMap, updateSumsMap, emptySumsMap)
import VWAP.Out (sumsMap2Report)


main :: IO ()
main = do

    -- start MVar with empty sumsMap
    sumsMapVar <- newMVar emptySumsMap

    backupApp <- Scotty.scottyApp $ doScotty sumsMapVar
    Warp.run 3000 $ websocketsOr WS.defaultConnectionOptions (wsApp sumsMapVar) backupApp

    where
        wsApp :: MVar SumsMap -> WS.ServerApp
        wsApp sumsMapVar pending_conn = do
            conn <- WS.acceptRequest pending_conn

            WS.receiveDataMessage conn >>= \case 
                WS.Text input _ -> do
                    let csvStream = CsvS.decode Csv.NoHeader input :: CsvS.Records Match
                    liftIO $ modifyMVar_ sumsMapVar $ \sumsMap ->
                        return $! F.foldl' updateSumsMap sumsMap csvStream
                WS.Binary _ -> undefined
            
            WS.sendTextData conn ("Hello, client!" :: Text)


doScotty :: MVar SumsMap -> Scotty.ScottyM ()
doScotty sumsMapVar = do

        Scotty.get "/summary" $ do

            -- Grab the latest sumsMap
            sumsMap <- liftIO $ readMVar sumsMapVar

            -- convert SumsMap into Report
            let report = sumsMap2Report sumsMap

            -- set response
            Scotty.json report
        
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
            liftIO $ modifyMVar_ sumsMapVar $ \sumsMap ->
                return $! F.foldl' updateSumsMap sumsMap csvStream

            -- Grab the latest sumsMap
            sumsMap <- liftIO $ readMVar sumsMapVar

            -- convert SumsMap into Report
            let report = sumsMap2Report sumsMap

            -- set response
            Scotty.json report


    where
        handleInterrupt :: SomeException -> IO ()
        handleInterrupt _ = return ()
