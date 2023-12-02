-- day01
import System.IO
import Data.Char
import Data.List.NonEmpty (NonEmpty)


lineValue :: [Char] -> Int
lineValue line = combined
    where
        digits = [ c | c <- line, isDigit c]
        
        first_digit :: Int
        first_digit = digitToInt (head digits)

        last_digit :: Int
        last_digit = digitToInt (last digits)
        
        combined :: Int
        combined = first_digit * 10 + last_digit



fileValue :: String -> Int
fileValue contents = sum lineValues
    where
        lineValues = [ (lineValue line) | line <- (lines contents)]


numbers = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]



-- solutions
testFile1 = "test1.txt"
testFile2 = "test2.txt"
inputFile = "input.txt"


solve1 :: String -> Int
solve1 contents = fileValue contents


solve2 :: String -> Int
solve2 contents = fileValue contents


----
main :: IO ()
main = do

    putStrLn "thinking..."
    

    testFile1Contents <- readFile testFile1
    testFile2Contents <- readFile testFile2
    inputFileContents <- readFile inputFile


    let testAnswer1 = solve1 testFile1Contents
    let inputAnswer1 = solve1 inputFileContents
    
    let testAnswer2 = solve2 testFile2Contents
    let inputAnswer2 = solve2 inputFileContents


    putStrLn ("test case 1: " ++ show testAnswer1)
    putStrLn ("answer 1: " ++ show inputAnswer1)

    putStrLn ("test case 2: " ++ show testAnswer2)
    putStrLn ("answer 2: " ++ show inputAnswer2)


    putStrLn "done!"
