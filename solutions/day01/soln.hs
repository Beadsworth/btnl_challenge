-- day01
module Solution.Soln  
( solve1  
, solve2  
) where 


import System.IO
import Data.Char
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe


lineValue :: String -> Int
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


numbers :: [String]
numbers = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
numbers2 :: [String]
numbers2 = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
numbersMatch :: [(String, String)]
numbersMatch = zip numbers numbers2


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


repl :: String -> String -> String -> String
repl line old new
    | findResult == Nothing = line
    | otherwise = repl newLine old new

    where
        findResult = find line old
        i = fromJust findResult
        newLine = (take i line) ++ new ++ (drop (i + (length old)) line)



-- replce middle chars only, keep first and last char
replMid :: String -> String -> String -> String
replMid line old new = repl line old new2
    where
        new2 = (take 1 old) ++ new ++ (drop ((length old) -1) old)


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
                newLine =  replMid line old new


-- solutions
solve1 :: String -> Int
solve1 contents = fileValue contents

-- the words overlap!
solve2 :: String -> Int
solve2 contents = fileValue (replAll contents)
