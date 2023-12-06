-- day00
module Solution.Soln  
( solve1  
, solve2  
) where 


-- https://stackoverflow.com/a/4981265
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'


-- winning numbers
toWinners :: String -> [Int]
toWinners line = nums
    where
        firstHalf = head (wordsWhen (=='|') line)
        winPart = last (wordsWhen (==':') firstHalf)
        numStrs = wordsWhen (==' ') winPart
        nums = [ read numStr | numStr <- numStrs]


-- ticket numbers
toTickets :: String -> [Int]
toTickets line = nums
    where
        lastHalf = last (wordsWhen (=='|') line)
        numStrs = wordsWhen (==' ') lastHalf
        nums = [ read numStr | numStr <- numStrs]


-- ticket numbers that match winning numbers
toMatches :: String -> [Int]
toMatches line = matches
    where
        w = toWinners line
        t = toTickets line
        matches = [ n | n <- w, n' <- t, n == n']


matchCount :: String -> Int
matchCount line = length (toMatches line)


-- score for each game
toScore :: String -> Int
toScore line
    | mCount < 1 = 0
    | otherwise = score
    where
        mCount = matchCount line
        expn = maximum [0, (mCount - 1)]
        score = 2 ^ expn


-- start at i = 0
copies :: [Int] -> [Int]
copies matchCounts = copiesHelper 0 matchCounts startCounts
    where
        l = length matchCounts
        startCounts = [ 1 | n <- [1..l] ]

        copiesHelper :: Int -> [Int] -> [Int] -> [Int]
        copiesHelper i matchCounts copyCounts
            | i >= l - 1 = copyCounts
            | otherwise = copiesHelper (i + 1) matchCounts newCopyCounts
            where
                n = matchCounts !! i 
                c = copyCounts !! i
                
                addList = [ 0 | _ <- [1..(i+1)]] ++ [ c | _ <- [1..n]] ++ [ 0 | _ <- [1..(l-n-i-1)]]
                
                newCopyCounts = map (\(f, s) -> f + s) (zip (copyCounts) (addList))


-- matches
--    [ 4 ,  2 ,  2 ,  1 ,  0 ,  0 ]


-- iterate example
-- -> [(1),  1 ,  1 ,  1 ,  1 ,  1 ]

-- i = 0
-- +  [ 0 ,  1 ,  1 ,  1 ,  1 ,  0 ]
-- -> [ 1 , (2),  2 ,  2 ,  2 ,  1 ]

-- i = 1
-- +  [ 0 ,  0 ,  2 ,  2 ,  0 ,  0 ]
-- -> [ 1 ,  2 , (4),  4 ,  2 ,  1 ]

-- i = 2
-- +  [ 0 ,  0 ,  0 ,  4 ,  4 ,  0 ]
-- -> [ 1 ,  2 ,  4 , (8),  6 ,  1 ]

-- i = 3
-- +  [ 0 ,  0 ,  0 ,  0 ,  8 ,  0 ]
-- -> [ 1 ,  2 ,  4 ,  8 ,(14),  1 ]

-- i = 4
-- +  [ 0 ,  0 ,  0 ,  0 ,  0 ,  0 ]
-- -> [ 1 ,  2 ,  4 ,  8 , 14 , (1)]


-- solutions
solve1 :: String -> Int
solve1 contents = sum scores
    where
        scores = [ toScore line | line <- (lines contents)]


solve2 :: String -> Int
solve2 contents = sum c
    where
        matchCounts = [ matchCount line | line <- (lines contents)]
        c = copies matchCounts
