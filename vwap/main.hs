import VWAP.In (Match (..), Side (..))
import System.IO (hGetLine, stdin, isEOF)
import Control.Monad (unless)
import Data.List.Split (splitOn)
import Data.Int (Int64)
import Data.Word (Word32)


parseLine :: String -> Match
parseLine line = match
    where
        [col0, col1, col2, col3, col4, col5] = splitOn "," line
        match = Match   { makerAcntID = col0
                        , takerAcntID = col1
                        , prodSym = col2
                        , takerSide = read col3
                        , price = read col4
                        , quantity = read col5
                        }


processLine :: String -> IO ()
processLine line = putStrLn parsedLine
    where
        parsedLine = show $ parseLine line


processStdin :: IO ()
processStdin = do

    eof <- isEOF
    unless eof $ do
        line <- hGetLine stdin
        processLine line
        processStdin  -- Recursively read the next line


main :: IO ()
main = do
    putStrLn "working..."

    processStdin

    putStrLn "done!"
