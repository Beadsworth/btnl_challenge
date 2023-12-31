-- day00
module Solution.Soln  
( solve1  
, solve2  
) where 



import Data.List.Split
import qualified Data.Text as T
import qualified Data.Map as Map


type SectionMap = Map.Map String [(Int, Int, Int)]


mapKeys = 
    [ "seed-to-soil"
    , "soil-to-fertilizer"
    , "fertilizer-to-water"
    , "water-to-light"
    , "light-to-temperature"
    , "temperature-to-humidity"
    , "humidity-to-location" ]


tuplify3 :: [a] -> (a,a,a)
tuplify3 [x,y,z] = (x,y,z)


parseMap :: String -> ([Int], SectionMap)
parseMap contents = (seeds, sectionMap)
    where
        noLastLine = T.unpack (T.strip (T.pack contents))
        sections = splitOn "\n\n" noLastLine
        cleanSections = [ (splitOn "\n" sec) | sec <- sections ]
        nonSeedSections = drop 1 cleanSections
        sectionPreMap = [ (head (take 1 sec), (drop 1 sec)) | sec <- nonSeedSections ]
        sectionPreMap2 = [ (head (splitOn " map:" k), [ [ read c :: Int | c <- (splitOn " " line) ] | line <- v]) | (k, v) <- sectionPreMap]
        sectionPreMap3 = [ (k, [tuplify3 x | x <- v ] ) | (k, v) <- sectionPreMap2 ]
        sectionMap = Map.fromList sectionPreMap3
        
        -- get seeds
        seedsLine = head sections
        seedsNums = last (splitOn "seeds: " seedsLine)
        seeds = [ read seed | seed <- (splitOn " " seedsNums) ] :: [Int]


checkNumRange :: Int -> Int -> Int -> Bool
checkNumRange sourceStart rangeLength sourceNum = withinRange
    where withinRange = (sourceStart <= sourceNum) && (sourceNum < (sourceStart + rangeLength))


mapLookup :: [(Int, Int, Int)] -> Int -> Int
mapLookup mapList sourceNum
    | (length mapList) == 0 = sourceNum
    | checkNumRange sourceStart rangeLength sourceNum = dest
    | otherwise = mapLookup subMapList sourceNum
        where
            subMapList = drop 1 mapList
            (destStart, sourceStart, rangeLength) = head mapList 
            dest = destStart + (sourceNum - sourceStart)


traverseMap :: SectionMap -> [String] -> Int -> Int
traverseMap sectionMap keyList input
    | (length keyList) < 1 = input
    | otherwise = traverseMap sectionMap subKeyList result
        where
            currentKey = head keyList
            subKeyList = drop 1 keyList
            mapList = sectionMap Map.! currentKey
            result = mapLookup mapList input



-- solution part 1
solve1 :: String -> Int
solve1 contents = result
    where
        (seeds, sectionMap) = parseMap contents
        
        getLocation :: (Int -> Int)
        getLocation = traverseMap sectionMap mapKeys
        
        seedLocations = [ getLocation seed | seed <- seeds ]
        result = minimum seedLocations

        

-- solution part 2
solve2 :: String -> Int
solve2 contents = 0
