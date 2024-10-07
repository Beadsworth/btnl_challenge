import VWAP.In (Match (..), Side (..))
import System.IO (hGetContents, stdin)
import Data.List.Split (splitOn)

-- Assuming the CSV has no headers and the second column contains the counts (integer)
main :: IO ()
main = do
    -- Read from stdin lazily
    input <- hGetContents stdin

    -- Split input into lines
    let csvLines = lines input

    -- Map over each line, parse the second field as an Int, and sum them
    let counts = map (read . (!! 5) . splitOn ",") csvLines :: [Int]
    let cumulativeSum = sum counts

    -- Print the cumulative sum of the second column
    print cumulativeSum




-- import Data.Csv
-- import GHC.Generics (Generic)
-- import qualified Data.ByteString.Lazy as BL
-- import qualified Data.Vector as V


-- -- Automatically generate a FromRecord instance for parsing CSV
-- instance FromRecord Person where
--   parseRecord v
--     | V.length v == 4 = Person <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3  -- Assuming count is the fourth column
--     | otherwise = fail "Invalid record length"


-- parseLine :: String -> Match
-- parseLine (a, b, c, d, e, f) = match
--     where
--         match = Match   { makerAcntID = a
--                         , takerAcntID = b
--                         , prodSym = c
--                         , takerSide = d
--                         , price = e
--                         , quantity = f
--                         }


-- ----
-- main :: IO ()
-- main = do

--     putStrLn "thinking..."


--     -- Read CSV data from stdin
--     csvData <- BL.getContents
--     lines = lines csvData
--     case decode NoHeader csvData of
--         Left err -> putStrLn ("Error: " ++ err)
--         Right v -> do


            

--             -- let cumulativeCount = V.foldl' (\acc person -> acc + count person) 0 v
--             putStrLn "end"
--     -- let match1 = Match  { makerAcntID = "Tyrell Corp A123"
--     --                     , takerAcntID = "Wayland-Yutani Corp BC32"
--     --                     , prodSym = "BUSU1"
--     --                     , takerSide = Bid
--     --                     , price = 42
--     --                     , quantity = 10
--     --                     }
--     -- let match2 = Match  { makerAcntID = "CHOAM Arakis Z23"
--     --                     , takerAcntID = "OPEC 897"
--     --                     , prodSym = "BUIZ1"
--     --                     , takerSide = Ask
--     --                     , price = (-2)
--     --                     , quantity = 14
--     --                     }
    
--     -- let match3 = Match  { makerAcntID = "InGen Tech BCZ232"
--     --                     , takerAcntID = "BioSynFG332"
--     --                     , prodSym = "BUSM2"
--     --                     , takerSide = Bid
--     --                     , price = 43250
--     --                     , quantity = 23
--     --                     }

--     -- contents <- getContents
--     -- putStrLn contents

--     putStrLn "done!"
