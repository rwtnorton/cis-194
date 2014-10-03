module Cis194.Hw.Week1 where

-------------
-- Ex 1-4  --
-------------

lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

dunno :: [Integer] -> Integer -> [Integer]
dunno acc x = if x < 10
              then x:acc
              else let x'   = dropLastDigit x
                       lst  = lastDigit x
                       acc' = lst:acc
                   in  dunno acc' x'

splitDigit :: Integer -> [Integer]
splitDigit n = [dropLastDigit n, lastDigit n]

toDigits :: Integer -> [Integer]
toDigits n = dunno [] n

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther _ = undefined

sumDigits :: [Integer] -> Integer
sumDigits _ = undefined

validate :: Integer -> Bool
validate _ = undefined

---------------------
-- Towers of Hanoi --
---------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi _ _ _ _ = undefined

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 _ _ _ _ _ = undefined
