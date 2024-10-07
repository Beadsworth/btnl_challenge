import VWAP.In (Match (..), parseCSVLine, updatePreReport, emptyPreReport, PreReport (..))
import System.IO (hGetLine, stdin, hIsEOF)
import Control.Monad (unless)

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
    putStrLn "working..."

    -- process entire CSV input
    cumPreReport <- processStdin emptyPreReport
    putStrLn $ show cumPreReport

    putStrLn "done!"
