import VWAP.In (Match (..), Side (..))


----
main :: IO ()
main = do

    putStrLn "thinking..."

    let match1 = Match  { makerAcntID = "Tyrell Corp A123"
                        , takerAcntID = "Wayland-Yutani Corp BC32"
                        , prodSym = "BUSU1"
                        , takerSide = Bid
                        , price = 42
                        , quantity = 10
                        }
    let match2 = Match  { makerAcntID = "CHOAM Arakis Z23"
                        , takerAcntID = "OPEC 897"
                        , prodSym = "BUIZ1"
                        , takerSide = Ask
                        , price = (-2)
                        , quantity = 14
                        }
    
    let match3 = Match  { makerAcntID = "InGen Tech BCZ232"
                        , takerAcntID = "BioSynFG332"
                        , prodSym = "BUSM2"
                        , takerSide = Bid
                        , price = 43250
                        , quantity = 23
                        }

    putStrLn (show match1)
    putStrLn (show match2)
    putStrLn (show match3)
    -- contents <- getContents
    -- putStrLn contents

    putStrLn "done!"
