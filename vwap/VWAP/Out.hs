-- Module For constructing output JSON

import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe (isJust, fromJust, catMaybes)
import Data.Word (Word32)


type MakerAcntID = Str
type TakerAcntID = Str
type ProdSym = Str

data TakerSide = Ask | Bid
    deriving (Eq, Ord, Show, Read)

type Price = Int64
-- Word32 is actually uint32
type Quantity = Word32
