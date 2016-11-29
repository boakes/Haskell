-- Define a datatype for citations in a paper. Citations can be a book, an article, or a website. Be
-- sure to include appropriate information.

type Date = String
--option 1
{-
data Citation = Book [String] String Sting Date | Journal [String] String String Date | Website [String] String String Date
--option 2
data CitationType = Book | Journal | Website
data Citation = Citation {ctype :: CitationType, authors :: [String], title ::  String, location :: String, date ::  Date }
-- option 3
data Citation = Citation {ctype :: CitationType, authors :: [String], title ::  String, url :: Maybe String, 
                          publisher :: Maybe String, journal :: Maybe String, date ::  Date }
-}
--option 4, the best
data Citation = Book { authors :: [String], title :: String, publisher ::  String, date ::  Date, pageNum :: Integer }
              | Journal { authors :: [String], title :: String, journal :: String, date :: Date }
              | Website { authors :: [String], title :: String, url :: String, date :: Date}



-- Poker hand 
data Suit = Heart | Spade | Club | Diamond deriving (Show, Eq)
--data Value = Ace | King | Queen | Jack | Ten | Nine
--data Card = Card Suit Integer deriving Show
type Card = (Suit, Integer)
--data Card = Heart Int | Spade Int | Club Int | Diamond Int deriving Show

--data Value = Ace | King | Queen | Jack | Ten | Nine
--data Hand = Poker Card Card Card Card Card deriving Show
type Hand = [Card]

flush :: Hand -> Bool
flush [(s1,r1), (s2,r2), (s3,r3), (s4,r4), (s5,r5)] = s1 == s2 && s1 == s3 && s1 == s4 && s1 == s5
flush (card:cards) = aux (fst card) cards
    where aux suit (card:cards) = (suit == (fst card)) && (aux suit cards)
flush (card:cards) = all (\c -> fst c == fst card) cards


-- Chemical reactions, which have inputs, outputs, possibly a minimum tempreature, and possibly an
-- enzyme. Consider one situation where you might have both, and one situation where they are
-- exclusive.
