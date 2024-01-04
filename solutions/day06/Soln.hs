-- day00
module Solution.Soln  
( solve1  
, solve2  
) where

import qualified Data.List.Split as Split
import qualified Data.Text as T


    -- d = v * t1
    -- v = t0
    -- t0 + t1 = tt
    -- we know d and tt
    -- need to know t0 & t1
    -- d = t0 * t1
    -- d = t0 * (tt - t0)
    -- d = t0*tt - t0^2
    -- t0^2 - tt*t0 + d = 0
    --
    -- using quadratic formula:
    -- a = 1
    -- b = (-1) * tt
    -- c = d
    --
    -- for each race, find the min & max times to hold the button
    -- then count the integers between those two times


parseInput :: String -> [(Int, Int)]
parseInput contents = races
    where
        noLastLine = T.unpack (T.strip (T.pack contents))
        [firstLine, secondLine] = Split.splitOn "\n" noLastLine
        
        parseLine :: String -> String -> [Int]
        parseLine line label = lineValuesClean
            where
                lineValues = Split.splitOn " " (last (Split.splitOn label line))
                lineValuesClean = [ read value :: Int | value <- lineValues, (length value) > 0]
        
        timeValues = parseLine firstLine "Time:"
        distanceValues = parseLine secondLine "Distance:"

        races = zip timeValues distanceValues


parseInput2 :: String -> [(Integer, Integer)]
parseInput2 contents = races
    where
        noLastLine = T.unpack (T.strip (T.pack contents))
        [firstLine, secondLine] = Split.splitOn "\n" noLastLine
        
        parseLine :: String -> String -> [Integer]
        parseLine line label = lineNum
            where
                lineValues = Split.splitOn " " (last (Split.splitOn label line))
                lineValuesClean = [ value :: String | value <- lineValues, (length value) > 0]
                lineNum = [read (concat lineValuesClean) :: Integer]
        
        timeValues = parseLine firstLine "Time:"
        distanceValues = parseLine secondLine "Distance:"

        races = zip timeValues distanceValues


quadFormula :: (Integral a) => a -> a -> a -> (Double, Double)
quadFormula a b c = (solution1, solution2)
    where
        bf = fromIntegral b
        af = fromIntegral a
        inside = fromIntegral (b ^ 2 - (4 * a * c))
        solution1 = ( ((-1)*bf) - inside ** 0.5) / (2 * af)
        solution2 = ( ((-1)*bf) + inside ** 0.5) / (2 * af)
        

isInt :: (Integral a, RealFrac b) => a -> b -> Bool
isInt n x = round (10 ^ fromIntegral n * (x - fromIntegral (round x))) == 0


findAnswer :: (Integral a) => [(a, a)] -> Int
findAnswer races = answer
    where
        calcPrecision = 10

        ceiling' :: Double -> Int
        ceiling' x
            | isInt calcPrecision x = firstPass + 1
            | otherwise = firstPass
            where
                firstPass = ceiling x
        
        floor' :: Double -> Int
        floor' x
            | isInt calcPrecision x = firstPass - 1
            | otherwise = firstPass
            where
                firstPass = floor x
    
        quadSolns = [ quadFormula 1 ((-1) * time) distance | (time, distance) <- races ]
        rounds = [ (ceiling' bottom, floor' top) | (bottom, top) <- quadSolns ]
        winningTimes = [ (top - bottom) + 1 | (bottom, top) <- rounds ]
        answer = product winningTimes


-- solution part 1
solve1 :: String -> Int
solve1 contents = answer
    where
        races = parseInput contents
        answer = findAnswer races


-- using quadratic formula from Wolfram Alpha online:
-- 8014959 - 53662612 + 1 = 45647654
-- solution part 2
solve2 :: String -> Int
solve2 contents = answer
    where
        races = parseInput2 contents
        answer = findAnswer races
