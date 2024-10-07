import VWAP.In (processCSVLine, emptyPreReport, PreReport)
import VWAP.Out (preReport2ReportJson)
import System.IO (hGetLine, stdin, hIsEOF)
import qualified Data.ByteString.Lazy.Char8 as B


-- Function to process stdin line by line and sum quantities
processStdin :: PreReport -> IO PreReport
processStdin preReport = do
    eof <- hIsEOF stdin
    if eof
        then return preReport
        else do
            line <- hGetLine stdin
            let newPreReport = processCSVLine preReport line
            processStdin newPreReport


main :: IO ()
main = do
    -- process entire CSV input, starting from empty PreReport
    preReport <- processStdin emptyPreReport
    -- convert PreReport into Report
    let reportJSON = preReport2ReportJson preReport
    -- print Report to stdout
    B.putStrLn reportJSON
