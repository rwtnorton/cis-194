module HW02 where

import Words
import Data.List

-- Though a Scrabble hand is the same Haskell type as a Scrabble word, they
-- have different properties. Specifically, a hand is unordered whereas a word
-- is ordered. We denote this distinction by using a type synonym to talk
-- about hands, even though we could just say `String`.
type Hand = [Char]

-- A `Template` is like a word, but it has '?' characters in some places as
-- placeholders for letters from a player's hand. Because real words do not
-- have '?' characters, we use another type synonym to track this distinction.
type Template = String

-- A 'STemplate' is like a template, but it has markers to indicate four kinds
-- of special board locations: double-letter noted with 'D', triple-letter
-- noted with 'T', double-word noted with '2', and triple-word noted with '3'.
-- For matching, these behave just like '?' does -- they can be filled in with
-- any letter. But, when scoring, any letter played on a 'D' gets double its
-- value, and any letter played on a 'T' gets triple its value. If any square
-- in the template is a '2', the whole word's value is doubled; if any square
-- in the template is a '3', the whole word's score is tripled. If multiple of
-- these special squares are in the same word, the effects multiply.
type STemplate = Template

-- Write your code below:
formableBy :: String -> Hand -> Bool
formableBy needle haystack = all isInHaystack needleH
  where needleH         = histo needle
        haystackH       = histo haystack
        isInHaystack hp = hasHistoPair hp haystackH

histo :: Ord a => [a] -> [(a, Int)]
histo cs = map (\s -> (head s, length s)) $ group $ sort cs

--hasHistoPair Ord a => (a, Int) -> [(a, Int)] -> Bool
--hasHistoPair :: Eq a => (a, t) -> [(a, b)] -> Bool
--hasHistoPair :: (Ord a1, Eq a) => (a, a1) -> [(a, a1)] -> Bool
hasHistoPair :: (Ord a1, Num a1, Eq a) => (a, a1) -> [(a, a1)] -> Bool
hasHistoPair (c,v) hps = case lookup c hps of
                         Just n  -> (v > 0) && (n > 0) && (v <= n)
                         Nothing -> False

test1 = formableBy "fun" "xnifuel" == True
test2 = formableBy "haskell" "klehals" == True
test3 = formableBy "haskell" "klehays" == False

wordsFrom :: Hand -> [String]
wordsFrom hand = filter (`formableBy` hand) allWords
