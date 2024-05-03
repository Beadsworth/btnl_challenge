-- day00
module Solution.Soln  
( solve1
, solve2
) where


import qualified Data.Text as Text
import qualified Data.Map as Map


type Path = String
type Node = String
type Fork = String
type NodeDef = (Node, Fork, Fork)
type NavMap = Map.Map Node (Fork, Fork)


-- line parsing
parseLine :: String -> NodeDef
parseLine line = (node, leftElement, rightElement)
    where
        words = map Text.unpack (Text.words (Text.pack line))
        [node, _, leftElementRaw, rightElementRaw] = words
        leftElement = take 3 (drop 1 leftElementRaw)
        rightElement = take 3 rightElementRaw
        

-- file parsing
parseFile :: String -> (Path, NavMap)
parseFile contents = result
    where 
        ls = lines contents
        [path] = take 1 ls
        nodes = map parseLine (drop 2 ls)
        navMap :: NavMap
        navMap = Map.fromList [ (node, (le, re)) | (node, le, re) <- nodes]
        result = (path, navMap)


pickFork :: (Fork, Fork) -> Char -> Fork
pickFork (le, re) 'L' = le
pickFork (le, re) 'R' = re


traverseNodesHelper :: (Node -> Bool) -> Node -> Path -> NavMap -> [Node]
traverseNodesHelper nodeIsEnd node path navmap 
    | nodeIsEnd node = [node] 
    | otherwise = node : traverseNodesHelper nodeIsEnd nextNode nextPath navmap
    where
        thisFork = head path
        nextPath = drop 1 path
        nextNode = pickFork (navmap Map.! node) thisFork


traverseNodesWrapper :: Path -> NavMap -> (Node -> Bool) -> Node -> Int
traverseNodesWrapper path navMap nodeIsEnd startNode = result
    where
        extendedPath = cycle path
        history = traverseNodesHelper nodeIsEnd startNode extendedPath navMap
        result = (length history) - 1


-- least common multiple
lcmm :: Integral a => [a] -> a
lcmm [] = 1
lcmm (x:xs) = lcm x (lcmm xs)


-- solution part 1
solve1 :: String -> Int
solve1 contents = result
    where
        (path, navMap) = parseFile contents
        -- start on "AAA"
        startNode = "AAA"
        -- end on "ZZZ"
        nodeIsEnd = (== "ZZZ")
        result = traverseNodesWrapper path navMap nodeIsEnd startNode


-- solution part 2
-- solve2 :: String -> Int
solve2 contents = result
    where
        (path, navMap) = parseFile contents
        -- stop on node ending in 'Z'
        nodeIsEnd = (\t -> last t == 'Z')
        -- find all nodes ending in 'A'
        startNodes = [key | key <- Map.keys navMap, (last key) == 'A']
        -- find length needed for each start node to finish
        pathLengths = map (traverseNodesWrapper path navMap nodeIsEnd) startNodes
        -- if paths converge to one point, the puzzle is not solvable
        -- if some paths do not finish, the puzzle is not solvable
        -- I guess that each path forms a loop
        -- so I just need to find the time when
        -- all loops complete simultaneously
        -- get least common multiple of all pathlengths
        result = lcmm pathLengths
