import qualified Solution.Soln as S (solve1, solve2)


-- input files
testFile1 = "Solution/test1.txt"
testFile2 = "Solution/test2.txt"
inputFile = "Solution/input.txt"


----
main :: IO ()
main = do

    putStrLn "thinking..."
    

    contents <- getContents
    putStrLn contents

    putStrLn "done!"
