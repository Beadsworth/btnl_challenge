-- day00
module Solution.Soln  
( solve1  
, solve2  
) where 


import Data.Char


find :: String -> String -> Maybe Int
find line subStr = findHelper line 0
    where
        subStrLength = length subStr
        findHelper ::  String -> Int -> Maybe Int
        findHelper lineH i
            | length currentSubLine < (length subStr)   = Nothing
            | currentSubLine == subStr                  = Just i
            | otherwise                                 = findHelper newSubLine (i+1)
            where
                currentSubLine = take subStrLength lineH
                newSubLine = drop 1 lineH


-- TODO: make this consuming
findAll :: String -> String -> [Int]
findAll line subStr = reverse indices
    where
        subStrLength = length subStr
        findAllHelper ::  String -> Int -> [Int] -> [Int]
        findAllHelper lineH i acc
            | length currentSubLine < (length subStr)   = acc
            | currentSubLine == subStr                  = findAllHelper newSubLine (i+1) (i:acc)
            | otherwise                                 = findAllHelper newSubLine (i+1) acc
            where
                currentSubLine = take subStrLength lineH
                newSubLine = drop 1 lineH
        indices = findAllHelper line 0 []


splitBy :: String -> String -> [String]
splitBy line subStr = reverse cleanSubStrings
    where
        splitIndices = findAll line subStr
        inPair = zip (-1 : splitIndices) (splitIndices ++ [length line])
        splitByHelper :: String -> [(Int, Int)] -> [String] -> [String]
        splitByHelper lineH [] acc = acc
        splitByHelper lineH ins acc = splitByHelper sublineH subIns (section : acc)
            where
                subStrLength = length subStr
                (start, stop) = head ins
                subIns = drop 1 ins
                count = stop - start - 1
                section = take count lineH
                sublineH = drop (count + subStrLength) lineH
        subStrings = splitByHelper line inPair []
        cleanSubStrings = [ s | s <- subStrings, (length s) > 0]


games :: String -> [String]
games contents = [ l | l <- (lines contents), (length l > 0) ]


gameNumber :: String -> Int
gameNumber game = read (last (gamePart `splitBy` "Game "))
    where
        gamePart = head (game `splitBy` ":")


gameRounds :: String -> [String]
gameRounds game = roundPart `splitBy` ";"
    where
        roundPart = last (game `splitBy` ":")


rgbCountRoundC :: String -> String -> Int
rgbCountRoundC round color = rCount
    where
        colorParts = round `splitBy` ","
        rs = [cp | cp <- colorParts, cp `find` color /= Nothing]
        
        rCount
            | length rs < 1 = 0
            | otherwise = read (head (firstString `splitBy` (" " ++ color)))
            where
                firstString = head rs


rgbCountRound :: String -> (Int, Int, Int) 
rgbCountRound round = (rCount, gCount, bCount)
    where
        colorCount :: String -> Int
        colorCount color = rgbCountRoundC round color

        rCount = colorCount "red"
        gCount = colorCount "green"
        bCount = colorCount "blue"


-- part 1
roundIsPossible :: String -> (Int, Int, Int) -> Bool
roundIsPossible round colorMax = (rC <= rM) && (gC <= gM) && (bC <= bM)
    where
        (rM, gM, bM) = colorMax
        (rC, gC, bC) = rgbCountRound round


gameIsPossible :: String -> (Int, Int, Int) -> Bool
gameIsPossible game colorMax = all (\round -> roundIsPossible round colorMax) rounds
    where
        rounds = gameRounds game


colorMaxValues = (12, 13, 14)


-- part 2
gameMaxC :: String -> (Int, Int, Int)
gameMaxC game = (rMax, gMax, bMax)
    where
        rounds = gameRounds game
        rgbCounts = [rgbCountRound round | round <- rounds]
        
        rMax = maximum [ rCount | (rCount, gCount, bCount) <- rgbCounts]
        gMax = maximum [ gCount | (rCount, gCount, bCount) <- rgbCounts]
        bMax = maximum [ bCount | (rCount, gCount, bCount) <- rgbCounts]


gamePower :: String -> Int
gamePower game = r * g * b
    where
        (r, g, b) = gameMaxC game


-- solutions
solve1 :: String -> Int
solve1 contents = sum [gameNumber game | game <- (games contents), gameIsPossible game colorMaxValues]


-- the words overlap!
solve2 :: String -> Int
solve2 contents = sum [gamePower game | game <- (games contents)]
