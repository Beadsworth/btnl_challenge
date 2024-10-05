import qualified Solution.Soln as S (solve1, solve2)


-- input files
testFile1 = "Solution/test1.txt"
testFile2 = "Solution/test2.txt"
inputFile = "Solution/input.txt"


----
main :: IO ()
main = do

    putStrLn "thinking..."
    

    testFile1Contents <- readFile testFile1
    testFile2Contents <- readFile testFile2
    inputFileContents <- readFile inputFile


    let testAnswer1 = S.solve1 testFile1Contents
    let inputAnswer1 = S.solve1 inputFileContents
    
    let testAnswer2 = S.solve2 testFile2Contents
    let inputAnswer2 = S.solve2 inputFileContents


    putStrLn ("test case 1: " ++ show testAnswer1)
    putStrLn ("answer 1: " ++ show inputAnswer1)

    putStrLn ("test case 2: " ++ show testAnswer2)
    putStrLn ("answer 2: " ++ show inputAnswer2)


    putStrLn "done!"
