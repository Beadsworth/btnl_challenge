import VWAP.In (Match (..), parseCSVLine, updatePreReport, emptyPreReport, PreReport (..))
import VWAP.Out (preReport2Json)
import System.IO (hGetLine, stdin, hIsEOF)
import qualified Data.ByteString.Lazy.Char8 as B



-- Process a single line from the CSV
processLine :: PreReport -> String -> PreReport
processLine preReport line = updatedPreReport
    where
        -- parse a single CSV line
        match = parseCSVLine line
        -- update cumulative preReport map with the match
        updatedPreReport = updatePreReport preReport match


-- Function to process stdin line by line and sum quantities
processStdin :: PreReport -> IO PreReport
processStdin preReport = do
    eof <- hIsEOF stdin
    if eof
        then return preReport
        else do
            line <- hGetLine stdin
            let newPreReport = processLine preReport line
            processStdin newPreReport


main :: IO ()
main = do
    -- process entire CSV input, starting from empty PreReport
    cumPreReport <- processStdin emptyPreReport
    -- convert PreReport into Report
    let reportJSON = preReport2Json cumPreReport
    -- print Report to stdout
    B.putStrLn reportJSON
