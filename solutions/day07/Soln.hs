-- day07
module Solution.Soln  
( solve1
, solve2
) where


import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.List as List


data Card = Joker | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Eq, Ord, Enum, Bounded, Show, Read)


data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
    deriving (Eq, Ord, Enum, Bounded, Show, Read)


data Hand = Hand Card Card Card Card Card
    deriving (Eq, Show)


type Bid = Int
data Play = Play Hand Bid
    deriving (Eq, Show)


-- create a key: value map for char: Card
keys :: [Char]
keys = [ '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A']

values :: Bool -> [Card]
values hasJoker
    | hasJoker == False = [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]
    | hasJoker == True = [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Joker, Queen, King, Ace]


-- cardDictionary :: Bool -> Map.Map
cardDictionary hasJoker = Map.fromList (zip keys (values hasJoker))


-- convert char to card
char2Card :: Bool -> Char -> Card
char2Card hasJoker cardChar = unpack lookup
    where
        lookup :: Maybe Card
        lookup = Map.lookup cardChar (cardDictionary hasJoker)
        
        unpack :: Maybe Card -> Card
        unpack Nothing = error "invalid card value"
        unpack (Just card) = card


-- convert hand to list of cards
hand2Cards :: Hand -> [Card]
hand2Cards (Hand c1 c2 c3 c4 c5) = [c1, c2, c3, c4, c5]


-- find repeating cards
getRepeats :: Hand -> (Int, Int)
getRepeats hand = (firstMostRepeats + jokerCount, secondMostRepeats)
    where
        cards = hand2Cards hand
        -- count cards, skip jokers
        counts = [length (filter (value ==) cards) | value <- [Two ..]]
        jokerCount = length (filter (Joker ==) cards)
        [firstMostRepeats, secondMostRepeats] = (take 2 . reverse . List.sort) counts


-- get the type of hand
getHandType :: Hand -> HandType
getHandType hand
    -- (firstMostRepeats, secondMostRepeats)
    | repeats == (5, 0) = FiveOfAKind
    | repeats == (4, 1)= FourOfAKind
    | repeats == (3, 2) = FullHouse
    | repeats == (3, 1) = ThreeOfAKind
    | repeats == (2, 2) = TwoPair
    | repeats == (2, 1) = OnePair
    | otherwise = HighCard
    where repeats = getRepeats hand


-- how to compare hands
compareHands :: Hand -> Hand -> Ordering
compareHands handA handB
    | handTypeA /= handTypeB = compare handTypeA handTypeB
    | handTypeA == handTypeB = compare cardsA cardsB
    where
        handTypeA = getHandType handA
        handTypeB = getHandType handB
        cardsA = hand2Cards handA
        cardsB = hand2Cards handB


-- Hand ordering
instance Ord Hand where
    compare handA handB = compareHands handA handB


-- Play ordering
instance Ord Play where
    compare (Play handA bidA) (Play handB bidB) = compare handA handB


-- line parsing
parsePlay :: Bool -> String -> Play
parsePlay hasJoker line = play
    where
        [hand_text, bid_text] = take 2 (Text.words (Text.pack line))
        bid :: Int
        bid = read (Text.unpack bid_text)
        chars = take 5 (Text.unpack hand_text)
        cards = map (char2Card hasJoker) chars
        [c1, c2, c3, c4, c5] = cards
        hand :: Hand
        hand = Hand c1 c2 c3 c4 c5
        play = Play hand bid


-- file parsing
parsePlays :: Bool -> String -> [Play]
parsePlays hasJoker contents = plays
    where 
        plays = [parsePlay hasJoker line | line <- (lines contents)]


-- solve the answers
countCards :: Bool -> String -> Int
countCards hasJoker contents = result
    where
        -- parse
        plays = parsePlays hasJoker contents
        -- order plays
        ordered_plays = List.sort plays
        -- combine bids with multipliers
        ordered_bids = [ bid | (Play hand bid) <- ordered_plays ]
        tuples = zip [1..] ordered_bids
        -- 1a + 2b + 3c + ...
        result = List.foldl' (\acc (x, y) -> acc + x * y) 0 tuples


-- solution part 1
solve1 :: String -> Int
solve1 contents = result
    where
        hasJoker = False
        result = countCards hasJoker contents


-- solution part 2
solve2 :: String -> Int
solve2 contents = result
    where
        hasJoker = True
        result = countCards hasJoker contents
