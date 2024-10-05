-- day00
module Solution.Soln  
( solve1
, solve2
) where


import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe (isJust, fromJust, catMaybes)
import qualified Data.Set as Set


-- The pipes are arranged in a two-dimensional grid of tiles:

-- | is a vertical pipe connecting north and south.
-- - is a horizontal pipe connecting east and west.
-- L is a 90-degree bend connecting north and east.
-- J is a 90-degree bend connecting north and west.
-- 7 is a 90-degree bend connecting south and west.
-- F is a 90-degree bend connecting south and east.
-- . is ground; there is no pipe in this tile.
-- S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.


data Pipe = NS | EW | NE | NW | SW | SE | Ground | Start
    deriving (Eq, Ord, Show, Read)

data Turn = LeftTurn | RightTurn | North | East | South | West
    deriving (Eq, Ord, Show, Read)

data Handedness = CW | NoLoop | CCW
    deriving (Eq, Ord, Show, Read)

type Row = [Pipe]
type PipeMap = Map.Map Int (Map.Map Int Pipe)
type Coords = (Int, Int)
type PipeLookup = (Coords -> Pipe)

data PipeLayout = PipeLayout PipeMap PipeLookup


-- create a key: value map for char: Card
keys :: [Char]
keys = [ '|', '-', 'L', 'J', '7', 'F', '.', 'S']

values :: [Pipe]
values = [NS, EW, NE, NW, SW, SE, Ground, Start]


-- cardDictionary :: Bool -> Map.Map
pipeDictionary = Map.fromList (zip keys values)
reversePipeDictionary = Map.fromList (zip values keys)


-- convert char to pipe
pipe2Char :: Pipe -> Char
pipe2Char pipe = unpack lookup
    where
        lookup :: Maybe Char
        lookup = Map.lookup pipe reversePipeDictionary
        
        unpack :: Maybe Char -> Char
        unpack Nothing = error "invalid pipe value"
        unpack (Just char) = char


-- convert pipe to char
char2Pipe :: Char -> Pipe
char2Pipe pipeChar = unpack lookup
    where
        lookup :: Maybe Pipe
        lookup = Map.lookup pipeChar pipeDictionary
        
        unpack :: Maybe Pipe -> Pipe
        unpack Nothing = error "invalid pipe value"
        unpack (Just pipe) = pipe


-- line parsing
parseLine :: String -> [(Int, Pipe)]
parseLine line = pipes
    where
        pipes = zip [0..] (map char2Pipe line)


-- file parsing
parseFile :: String -> [(Int, [(Int, Pipe)])]
parseFile contents = result
    where 
        ls = lines contents
        x_maps = map parseLine ls
        -- result = [ (x, y, pipe) | (y, x_map) <- (zip [0..] x_maps), (x, pipe) <- x_map ]
        result = zip [0..] x_maps


parsePipeMap :: String -> PipeMap
parsePipeMap contents = result
    where
        pipeList = parseFile contents
        result = Map.fromList [ (y, Map.fromList subList) | (y, subList) <- pipeList]


-- get pipe finding function from PipeMap
getPipeLookup :: PipeMap -> (Coords -> Pipe)
getPipeLookup pipeMap = pipeLookup
    where
        pipeLookup :: Coords -> Pipe
        pipeLookup (x, y) = pipeMap Map.! y Map.! x


-- return a list of neighborCoords
neighborCoords :: Coords -> Pipe -> [Coords]
neighborCoords (x, y) NS = [(x, y-1), (x, y+1)]
neighborCoords (x, y) EW = [(x-1, y), (x+1, y)]
neighborCoords (x, y) NE = [(x, y-1), (x+1, y)]
neighborCoords (x, y) NW = [(x, y-1), (x-1, y)]
neighborCoords (x, y) SW = [(x-1, y), (x, y+1)]
neighborCoords (x, y) SE = [(x+1, y), (x, y+1)]
neighborCoords (x, y) Ground = []
neighborCoords (x, y) Start = [(x-1, y), (x, y-1), (x+1, y), (x, y+1)]


getMapSize :: PipeMap -> (Int, Int)
getMapSize pipeMap = (xSize, ySize)
    where
        xSize = length (pipeMap Map.! 0)
        ySize = length pipeMap


getNeighbors :: PipeLayout -> Coords -> [Coords]
getNeighbors (PipeLayout pipeMap pipeLookup) (x, y) = filter hasGoodCoords ns
    where
        (xSize, ySize) = getMapSize pipeMap
        xMax = xSize - 1
        yMax = ySize - 1
        pipe = pipeLookup (x, y)
        ns = neighborCoords (x, y) pipe
        hasGoodCoords :: Coords -> Bool
        hasGoodCoords (x, y)
            | x < 0 = False
            | x > xMax = False
            | y < 0 = False
            | y > yMax = False
            |otherwise = True


-- kinda looks redundant now that I think about it...
connections :: PipeLayout -> Coords -> [Coords]
connections pipeLayout (x, y) = myConnections
    where
        -- curry getNeighbors with pipeMap
        neighbors :: Coords -> [Coords]
        neighbors = getNeighbors pipeLayout

        -- places I can go
        myNeighbors = neighbors (x, y)
        
        -- places I can go
        --  that are connected to me
        myConnections = filter (\mn -> (x, y) `elem` neighbors mn) myNeighbors


traversePipesHelper :: PipeLayout -> Coords -> Coords -> Maybe Coords -> Maybe [Coords]
traversePipesHelper _ _ _ Nothing = Nothing
traversePipesHelper pipeLayout start from (Just to)
    -- consider the remaining path ahead...
    -- if you loop back to the start, stop recording the history
    | to == start = Just []
    | otherwise = case traversePipesHelper pipeLayout start newFrom newTo of
                    -- if you're at a dead-end, this journey is useless
                    Nothing -> Nothing
                    -- otherwise, add your position to the history
                    Just path -> Just (to : path)
    where
        -- take one step
        newFrom = to
        -- find all connection to the next step
        conns = connections pipeLayout to
        -- remove current position from list of connections
        toChoices = filter (/= from) conns
        -- only take a step if it's not a dead-end
        newTo = case toChoices of
                    [] -> Nothing
                    (x:_) -> Just x


traversePipes :: PipeLayout -> Coords -> [[Coords]]
traversePipes pipeLayout start = result
    where
        traversePipesHelperCurry coords = traversePipesHelper pipeLayout start start (Just coords)
        conns = connections pipeLayout start
        result = map fromJust (filter isJust (map traversePipesHelperCurry conns))


pathLength :: [[Coords]] -> Maybe Int
pathLength [] = Nothing
pathLength (x:xs) = Just (((length x) + 1) `div` 2)


findStart :: PipeLayout -> Maybe Coords
findStart (PipeLayout pipeMap pipeLookup) = result
    where
        (xSize, ySize) = getMapSize pipeMap
        xMax = xSize - 1
        yMax = ySize - 1
        start = take 1 [ (x, y) | x <- [0..xMax], y <- [0..yMax], pipeLookup (x, y) == Start ]
        result = case start of
            [] -> Nothing
            [x] -> Just x


-- print map with some transformations
grid2String :: PipeLayout -> (Coords -> Char) -> String
grid2String (PipeLayout pipeMap pipeLookup) pred = outputString
    where
        lines = [ [ pred (x, y) | (x, lineMapInner) <-  Map.toList lineMapOuter] | (y, lineMapOuter) <- Map.toList pipeMap ]
        lines' = 0
        outputString = unlines lines


countCCW :: Turn -> Int
countCCW LeftTurn = 1
countCCW RightTurn = -1
countCCW _ = 0


getTurnsHandedness :: [Turn] -> Handedness
getTurnsHandedness turns = handedness
    where
        sumCCW = sum (map countCCW turns)
        handedness
            | sumCCW > 0 = CCW
            | sumCCW < 0 = CW
            | otherwise = NoLoop


getPathHandedness :: PipeLayout -> [Coords] -> Handedness
getPathHandedness pipeLayout path = getTurnsHandedness (getTurns pipeLayout path)


getTurns :: PipeLayout -> [Coords] -> [Turn]
getTurns pipeLayout path = turns
    -- assume first coords is start
    where
        starts = path
        mids = take (length path) (drop 1 (cycle path))
        ends = take (length path) (drop 2 (cycle path))
        deltas = [((x1 - x0, y1 - y0), (x2 - x1, y2 - y1)) | ((x0, y0), (x1, y1), (x2, y2)) <- zip3 starts mids ends]
        
        getTurnsHelper :: Coords -> Coords -> Turn
        -- straight path
        getTurnsHelper (1, 0) (1, 0) = East
        getTurnsHelper (-1, 0) (-1, 0) = West
        getTurnsHelper (0, -1) (0, -1) = North
        getTurnsHelper (0, 1) (0, 1) = South
        -- left turns
        getTurnsHelper (1, 0) (0, -1) = LeftTurn
        getTurnsHelper (0, 1) (1, 0) = LeftTurn
        getTurnsHelper (-1, 0) (0, 1) = LeftTurn
        getTurnsHelper (0, -1) (-1, 0) = LeftTurn
        -- right turns
        getTurnsHelper (1, 0) (0, 1) = RightTurn
        getTurnsHelper (0, 1) (-1, 0) = RightTurn
        getTurnsHelper (-1, 0) (0, -1) = RightTurn
        getTurnsHelper (0, -1) (1, 0) = RightTurn
        -- others
        getTurnsHelper delta0 delta1 = error ("bad tile")
        
        turns = map (\(d0, d1) -> getTurnsHelper d0 d1) deltas


-- assume coord is inside valid area
getLeftSide :: Turn -> Pipe -> Coords -> [Coords]
getLeftSide North pipe (x, y) = [(x-1, y)]
getLeftSide East pipe (x, y) = [(x, y-1)]
getLeftSide South pipe (x, y) = [(x+1, y)]
getLeftSide West pipe (x, y) = [(x, y+1)]
-- RightTurns have left-side tiles on the outside edge
getLeftSide RightTurn NE (x, y) = [(x-1, y), (x, y+1)]
getLeftSide RightTurn SE (x, y) = [(x-1, y), (x, y-1)]
getLeftSide RightTurn SW (x, y) = [(x+1, y), (x, y-1)]
getLeftSide RightTurn NW (x, y) = [(x+1, y), (x, y+1)]
-- left turns have no adjacent left-side tiles
-- Ground and Start have no left-side tiles
getLeftSide turn pipe (x, y) = []


getLeftSides :: PipeLayout -> [Coords] -> [Coords]
getLeftSides pipeLayout path = emptyLefts
    where
        PipeLayout pipeMap pipeLookup = pipeLayout
        turns = getTurns pipeLayout path
        pipes = map pipeLookup path
        turnsPipesPath = zip3 turns (drop 1 pipes) (drop 1 path)
        lefts = concat (map (\(t, p, c) -> getLeftSide t p c) turnsPipesPath)
        uniqueLefts = List.nub lefts
        emptyLefts = filter (\l-> not (elem l path)) uniqueLefts


-- assume all coords are valid
getInsideAdjacent :: Coords -> [Coords]
getInsideAdjacent (x, y) = adjacentList
    where
        adjacentList = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]


countCenters :: [Coords] -> [Coords] -> [Coords]
countCenters outsideCoords nonActiveCoords
    | length nonActiveAdjacents < 1 = []
    | otherwise = nonActiveAdjacents ++ (countCenters nonActiveAdjacents newNonActiveCoords)
    where
        uniqueAdjacents = List.nub (concat (map getInsideAdjacent outsideCoords))
        actualAdjacents = filter (\c -> not (c `elem` outsideCoords)) uniqueAdjacents
        nonActiveAdjacents = filter (\c -> c `elem` nonActiveCoords) actualAdjacents
        -- shrink the amount of nonActiveCoords
        newNonActiveCoords = filter (\c -> not (c `elem` nonActiveAdjacents)) nonActiveCoords


-- solution part 1
solve1 :: String -> Int
solve1 contents = result
    where
        pipeMap = parsePipeMap contents
        pipeLookup = getPipeLookup pipeMap
        pipeLayout = PipeLayout pipeMap pipeLookup
        start = fromJust (findStart pipeLayout)
        paths = traversePipes pipeLayout start        
        result = fromJust (pathLength paths)


-- solution part 2
-- solve2 :: String -> Int
solve2 contents = result
    where
        pipeMap = parsePipeMap contents
        pipeLookup = getPipeLookup pipeMap
        pipeLayout = PipeLayout pipeMap pipeLookup
        start = fromJust (findStart pipeLayout)

        -- first, find paths
        paths = traversePipes pipeLayout start
        completePaths = map (start :) paths

        -- find the counter-clockwise path
        -- you can do this by counting the number of left/right turns
        -- ccwPath = head (filter (\p -> (getPathHandedness pipeLayout p) == CCW) completePaths)
        ccwPath = head (filter (\p -> getPathHandedness pipeLayout p == CCW) completePaths)
        leftSides = getLeftSides pipeLayout ccwPath
        allTiles = [ (x, y) | (y, lineMapOuter) <- Map.toList pipeMap, (x, lineMapInner) <-  Map.toList lineMapOuter ]
        isntActive :: Coords -> Bool
        isntActive coords
            | coords `elem` ccwPath = False
            | coords `elem` leftSides = False
            | otherwise = True

        -- find all tiles touching the "left" side of the pipe/path
        -- non-active-pipe tiles touching the "left" side of the loop are inside
        -- this generates a new set of "left" points
        -- again, find the non-active-pipe tiles that are touching the (new) set of "left" points
        -- iterate until no more tiles touch the left

        nonActiveTiles = filter isntActive allTiles
        adjacentTiles1 = countCenters leftSides nonActiveTiles
        result = (length leftSides) + (length adjacentTiles1)

        -- print screen for debugging
        lefty :: Coords -> Char
        lefty coords
            | coords `elem` leftSides = '0'
            | coords `elem` adjacentTiles1 = '1'
            | coords `elem` nonActiveTiles = 'X'
            | otherwise = ' '
        
        debug = grid2String pipeLayout lefty
