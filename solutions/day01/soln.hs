-- day01
import System.IO
import Data.Char
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe


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
numbers2 :: [String]
numbers2 = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
numbersMatch :: [(String, String)]
numbersMatch = zip numbers numbers2
maxNumberLength = maximum [ length s | s <- numbers ]


-- startsWith :: String -> String -> Bool
-- startsWith line ns
--     | length line < length ns   = False
--     | line == ns        = True
--     | otherwise         = startsWith subLine ns
--     where
--         subLine = take ((length line) - 1) line


find ::  String -> String -> Maybe Int
find line ns = findHelper line ns i
    where
        i = 0

        findHelper ::  String -> String -> Int -> Maybe Int
        findHelper lineH nsH iH
            | length currentSubLine < (length nsH)  = Nothing
            | currentSubLine == nsH                 = Just iH
            | otherwise                             = findHelper newSubLine nsH newI
            where
                currentSubLine = take (length nsH) lineH
                newSubLine = drop 1 lineH
                newI = iH + 1


-- repl :: String -> String -> String -> String
-- repl line old new
--     | findResult == Nothing = line
--     | otherwise = repl newLine old new

--     where
--         findResult = find line old
--         i = fromJust findResult
--         newLine = (take i line) ++ new ++ (drop (i + (length old)) line)

repl2 :: String -> String -> String -> String
repl2 line old new
    | findResult == Nothing = line
    | otherwise = repl2 newLine old new

    where
        findResult = find line old
        i = fromJust findResult
        -- hacky way to allow overlap
        newS = (take 1 old) ++ new ++ (drop ((length old) - 1) old)
        newLine = (take i line) ++ newS ++ (drop (i + (length old)) line)


replAll :: String -> String
replAll line = replAllNumbers line numbersMatch
    where
        replAllNumbers :: String -> [(String, String)] -> String
        replAllNumbers line list
            | length list < 1 = line
            | otherwise = replAllNumbers newLine newList
            where
                (old, new) = head list
                newList = drop 1 list 
                newLine = repl2 line old new


-- hasSDigit :: String -> Bool
-- hasSDigit line = or [ contains line number | number <- numbers ]


-- solutions
testFile1 = "test1.txt"
testFile2 = "test2.txt"
inputFile = "input.txt"


solve1 :: String -> Int
solve1 contents = fileValue contents

-- the words overlap!
solve2 :: String -> Int
solve2 contents = fileValue (replAll contents)


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
