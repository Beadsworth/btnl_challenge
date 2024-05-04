-- day00
module Solution.Soln  
( solve1
, solve2
) where


import qualified Data.Text as Text


type Row = [Int]


-- line parsing
parseLine :: String -> Row
parseLine line = nums
    where
        words = map Text.unpack (Text.words (Text.pack line))
        nums :: [Int]
        nums = map read words


-- file parsing
parseFile :: String -> [Row]
parseFile contents = result
    where 
        ls = lines contents
        result = map parseLine ls


-- diff between neighboring numbers
diff :: Row -> Row
diff (x:[]) = []
diff (x1:x2:xs) = (x2 - x1):(diff (x2:xs))


-- determine if row has only zeros
hasAllZeros :: Row -> Bool
hasAllZeros row = (sum [ 1 | n <- row, n /= 0]) == 0


-- solve one row
solveRow :: Row -> Int
solveRow row
    | hasAllZeros row = 0
    | otherwise = lastNum + (solveRow newRow)
    where
        lastNum = last row
        newRow = diff row


-- solve one row
solveRow2 :: Row -> Int
solveRow2 row
    | hasAllZeros row = 0
    | otherwise = firstNum - (solveRow2 newRow)
    where
        firstNum = head row
        newRow = diff row


-- solution part 1
solve1 :: String -> Int
solve1 contents = result
    where
        rows = parseFile contents
        result = sum (map solveRow rows)


-- solution part 2
solve2 :: String -> Int
solve2 contents = result
    where
        rows = parseFile contents
        result = sum (map solveRow2 rows)
