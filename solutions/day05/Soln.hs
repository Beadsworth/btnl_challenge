-- day00
module Solution.Soln  
( solve1  
, solve2  
) where 



import Data.List.Split
import qualified Data.Text as T
import qualified Data.Map as Map


specialStrings = 
    [ "seeds:"
    , "seed-to-soil map:"
    , "soil-to-fertilizer map:"
    , "fertilizer-to-water map:"
    , "water-to-light map:"
    , "light-to-temperature map:"
    , "temperature-to-humidity map:"
    , "humidity-to-location map:" ]



parseMap :: String -> ([Int], Map.Map String [[Int]])
parseMap contents = (seeds, sectionMap)
    where
        noLastLine = T.unpack (T.strip (T.pack contents))
        sections = splitOn "\n\n" noLastLine
        cleanSections = [ (splitOn "\n" sec) | sec <- sections ]
        nonSeedSections = drop 1 cleanSections
        sectionPreMap = [ (head (take 1 sec), (drop 1 sec)) | sec <- nonSeedSections ]
        sectionPreMap2 = [ (head (splitOn " map:" k), [ [ read c :: Int | c <- (splitOn " " line) ] | line <- v]) | (k, v) <- sectionPreMap]
        sectionMap = Map.fromList sectionPreMap2
        
        -- get seeds
        seedsLine = head sections
        seedsNums = last (splitOn "seeds: " seedsLine)
        seeds = [ read seed | seed <- (splitOn " " seedsNums) ] :: [Int] 


-- solution part 1
-- solve1 :: String -> a
solve1 contents = seeds
    where
        (seeds, sectionMap) = parseMap contents
        
        

-- solution part 2
solve2 :: String -> Int
solve2 contents = 0
