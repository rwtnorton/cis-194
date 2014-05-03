module Cis194.Hw.Week1 where

-------------
-- Ex 1-4  --
-------------

toDigits :: Integer -> [Integer]
toDigits x = toDigitsTCORec x []

toDigitsTCORec :: Integer -> [Integer] -> [Integer]
toDigitsTCORec x xs
    | x <= 0    = xs
    | otherwise = toDigitsTCORec (x `div` 10) xs ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = xs

sumDigits :: [Integer] -> Integer
sumDigits _ = 0

validate :: Integer -> Bool
validate _ = False

---------------------
-- Towers of Hanoi --
---------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi _ _ _ _ = []

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 _ _ _ _ _ = []
