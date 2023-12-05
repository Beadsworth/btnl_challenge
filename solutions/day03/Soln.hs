-- day00
module Solution.Soln  
( solve1  
, solve2  
) where 


import Data.Array (Array, (//), (!))
import qualified Data.Array as A
import qualified Data.Char as C
import Data.Set
import Data.List as L


-- https://stackoverflow.com/a/18627837
mkUniq :: Ord a => [a] -> [a]
mkUniq = toList . fromList


data Grid = Grid (Array Int (Array Int Char))


-- turn input into array
toGrid :: String -> Grid
toGrid contents = Grid grid
    where
        lns = lines contents
        rows = [ A.listArray (0, (length ln) - 1) ln | ln <- lns ]
        grid = A.listArray (0, ((length lns) - 1)) rows


height :: Grid -> Int
height (Grid grid) = diff + 1
    where
        (first, last) = A.bounds grid
        diff = abs (last - first)


width :: Grid -> Int
width (Grid grid) = diff + 1
    where
        -- firstRow :: Grid -> Array Int Char
        firstRow g = g ! 0
        (first, last) = A.bounds (firstRow grid)
        diff = abs (last - first)


-- (0, 0) is top-left corner
-- x up -> down
-- y up -> right
point :: (Int, Int) -> Grid -> Char
point (x, y) (Grid grid) = grid ! y ! x


indexList :: Grid -> [(Int, Int)]
indexList grid = iList
    where
        p (x, y) = point (x, y) grid
        w = width grid
        h = height grid
        iList = [ (x, y) | x <- [0..(w-1)], y <- [0..(h-1)] ]


-- find all number-starting-indices
numStartIndices :: Grid -> [(Int, Int)]
numStartIndices grid = iList
    where
        p (x, y) = point (x, y) grid
        pIsDigit (x, y) = C.isDigit (p (x, y))

        isNumStart :: (Int, Int) -> Bool
        isNumStart (x, y)
            | (pIsDigit (x, y)) && (x == 0) = True
            | (pIsDigit (x, y)) && (not (pIsDigit (x-1, y))) = True
            | otherwise = False

        iList = [ (x, y) | (x, y) <- indexList grid, isNumStart (x, y) ]
        


-- recursively find numbers left->right
rNumI :: (Int, Int) -> Grid -> [(Int, Int)]
rNumI (x, y) grid = iList
    where
        w = width grid
        p (x, y) = point (x, y) grid
        pIsDigit (x, y) = C.isDigit (p (x, y))
        
        -- rNumHelperLeft :: (Int, Int) -> String
        -- rNumHelperLeft (x, y)
        --     | (x<0) || (not (pIsDigit(x, y))) = ""
        --     | otherwise = (rNumHelperLeft (x-1, y)) ++ (p (x, y))

        rNumIR :: (Int, Int) -> [(Int, Int)]
        rNumIR (x, y)
            | (x>=w) || (not (pIsDigit (x, y))) = []
            | otherwise = (x, y):(rNumIR (x+1, y))
        
        iList = rNumIR (x, y)


-- convert number-starting-index to Int
rNumInt :: (Int, Int) -> Grid -> Int
rNumInt (x, y) grid = num
    where
        p (x, y) = point (x, y) grid
        iList = rNumI (x, y) grid
        numStr = [ p (x, y) | (x, y) <- iList]
        num = read numStr


isSymbol :: Char -> Bool
isSymbol char = (not (C.isDigit char)) && ('.' /= char)


isGear :: Char -> Bool
isGear char = '*' == char


-- does the given index touch a symbol?
indexTouchesSomething :: (Int, Int) -> Grid -> (Char -> Bool) -> (Bool, [(Int, Int)])
indexTouchesSomething (x1, y1) grid f = (isTouching, touchList)
    where
        h = height grid
        w = width grid
        p (x, y) = point (x, y) grid

        -- filter to Chars that are symbols
        pF (x, y) = f (p (x, y))
        -- find all surrounding points
        eight (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1), (x-1, y-1), (x-1, y+1), (x+1, y-1), (x+1, y+1)]
        -- make sure points are symbols and within bounds
        goodEight (x, y) = [ (x', y') | (x', y') <- eight (x, y), 0<=x', x'<h, 0<=y', y'<w, pF (x', y')]
        -- at least one match -> touching
        touchList = goodEight (x1, y1) :: [(Int, Int)]
        isTouching = (length touchList) > 0 :: Bool


-- f is some test for touching
numIsTouching :: [(Int, Int)] -> Grid -> (Char -> Bool) -> (Bool, [(Int, Int)])
numIsTouching indexList grid f = (isTouchingSomething, hitList)
    where
        isTouching (x, y) = fst (indexTouchesSomething (x, y) grid f)
        touchList (x, y) = snd (indexTouchesSomething (x, y) grid f)
        hitList = mkUniq (concat [ touchList (x, y) | (x, y) <- indexList, isTouching (x, y) ])
        isTouchingSomething = (length hitList) > 0


numIs :: Grid -> [[(Int, Int)]]
numIs grid = iNums
    where
        iList = numStartIndices grid
        iNums = [ rNumI (x, y) grid | (x, y) <- iList ]


-- sum of all numbers touching a symbol
gridSum :: Grid -> Int
gridSum grid = sum matchingNums
    where
        iNums = numIs grid
        num iNum = rNumInt (head iNum) grid
        isMatch iNum = numIsTouching iNum grid isSymbol
        matchingNums = [ num iNum | iNum <- iNums, let (isTouchingSomething, hitList) = isMatch iNum in isTouchingSomething]


-- sum all products of nums touching gears
gearRatioSum :: Grid -> Int
gearRatioSum grid = result
    where
        iNums = numIs grid
        num iNum = rNumInt (head iNum) grid
        isMatch iNum = numIsTouching iNum grid isGear
        -- num -> gear map
        matchingNums = [ (num iNum, snd (isMatch iNum)) | iNum <- iNums, fst (isMatch iNum)]
        gearMap = sort [ (gear, num) | (num, gears) <- matchingNums, gear <-gears ]
        gearGroups = groupBy (\x y -> fst x == fst y) gearMap
        gearGroups2 = L.filter (\g -> (length g) == 2) gearGroups
        ratios = [ product [ num | (_, num) <- gearGroup] | gearGroup <- gearGroups2 ]
        result = sum ratios


-- solutions
solve1 :: String -> Int
solve1 contents = gridSum grid
    where
        grid = toGrid contents


solve2 :: String -> Int
solve2 contents = gearRatioSum grid
    where
        grid = toGrid contents
