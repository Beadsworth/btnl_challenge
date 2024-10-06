import VWAP.In (Match (..), Side (..))


----
main :: IO ()
main = do

    putStrLn "thinking..."

    let match = Match { makerAcntID="maker", takerAcntID="taker", prodSym="prod sym", takerSide=Ask, price=1000, quantity=1}

    putStrLn (show match)
    -- contents <- getContents
    -- putStrLn contents

    putStrLn "done!"
