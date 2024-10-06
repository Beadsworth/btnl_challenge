-- Module For Parsing input CSV

module VWAP.In  
( Side (..)
, Match (..)
) where

import Data.Maybe (isJust, fromJust, catMaybes)
import Data.Int (Int64)
import Data.Word (Word32)


data Side = Ask | Bid
    deriving (Eq, Ord, Show, Read)


data Match = Match
    { makerAcntID :: String
    , takerAcntID :: String
    , prodSym :: String
    , takerSide :: Side
    , price :: Int64
    , quantity :: Word32
    } deriving (Show) 
