import VWAP.In (Match (..), parseCSVLine, updatePreReport, emptyPreReport, PreReport (..))
import System.IO (hGetLine, stdin, isEOF)
import Control.Monad (unless)

-- Process a single line from the CSV
processLine :: PreReport -> String -> PreReport
processLine preReport line = updatedPreReport
    where
        -- parse a single CSV line
        match = parseCSVLine line
        -- update cumulative preReport map with the match
        updatedPreReport = updatePreReport preReport match

-- Recursively read stdin line-by-line
-- until EOF is reached
processStdin :: PreReport -> IO PreReport
processStdin preReport = do
    eof <- isEOF
    unless eof $ do
        line <- hGetLine stdin
        let newPreReport = processLine preReport line
        -- pass updated preReport to next call
        processStdin newPreReport

main :: IO ()
main = do
    putStrLn "working..."

    let preReport = emptyPreReport
    -- process entire CSV input
    cumPreReport <- processStdin preReport
    putStrLn $ show cumPreReport

    putStrLn "done!"
