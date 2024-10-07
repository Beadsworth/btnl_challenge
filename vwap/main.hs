import VWAP.In (Match (..), parseCSVLine, updatePreReport, emptyPreReport, PreReport (..))
import System.IO (hGetLine, stdin, isEOF)
import Control.Monad (unless)


processLine :: PreReport -> String -> PreReport
processLine preReport line = updatedPreReport
    where
        -- parse a single CSV line
        match = parseCSVLine line
        -- update cumulative preReport map with the match
        updatedPreReport = updatePreReport preReport match


-- Recursively read stdin line-by-line
-- until EOF is reached
processStdin :: PreReport -> PreReport
processStdin preReport = do
    eof <- isEOF
    unless eof $ do
        line <- hGetLine stdin
        processLine preReport line
        processStdin preReport


main :: IO ()
main = do
    putStrLn "working..."

    let preReport = emptyPreReport
    let cumPreReport = processStdin preReport
    putStrLn $ show cumPreReport

    putStrLn "done!"
