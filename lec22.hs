data Suit = Clubs | Diamonds | Hearts | Spades deriving (Show, Eq, Ord, Read)
data Rank = King | Queen | Jack | Number Integer deriving (Show, Eq)

prettyPrint :: Rank -> String
prettyPrint King = "King"
prettyPrint Queen = "Queen"
prettyPrint Jack = "Jack"
prettyPrint (Number 1) = "Ace"
prettyPrint (Number x) = show x

instance Ord Rank where
    --compare :: Rank -> Rank -> Ordering
    compare (Number 1) (Number 1) = EQ
    compare (Number 1) _ = GT
    compare _ (Number 1) = LT

    compare King King = EQ
    compare King _ = GT
    compare _ King = LT

    compare Queen Queen = EQ
    compare Queen _ = GT
    compare _ Queen = LT

    compare Jack Jack = EQ
    compare Jack _ = GT
    compare _ Jack = LT

    compare (Number x) (Number y) = compare x y

data Card = Card Rank Suit deriving (Eq)
instance Ord Card where
    (<=) (Card r1 s1) (Card r2 s2) = 
            case compare r1 r2 of
                LT -> True
                GT -> False
                EQ -> s1 <= s2 
    (<=) (Card r1 s1) (Card r2 s2) 
        | (r1 == r2) = s1 <= s2
        | otherwise = r1 < r2
    compare (Card r1 s1) (Card r2 s2) = 
            case compare r1 r2 of
                EQ -> compare s1 s2
                LT -> LT 
                GT -> GT

instance Show Card where
    --show :: Card -> String
    show (Card r s) = (prettyPrint r) ++ " of " ++ (show s)

readCard :: String -> Card
readCard str = 
    case words str of
        [rank, "of", suit] ->  Card (readRank rank) (read suit)
        _ -> error "Can't read!"

readRank :: String -> Rank
readRank "Jack" = Jack
readRank "Queen" = Queen
readRank "King" = King
readRank "Ace" = Number 1
readRank x = Number (read x)
