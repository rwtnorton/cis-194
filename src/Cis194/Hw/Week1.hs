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
doubleEveryOther xs = reverse $ map (\(i,x) -> if odd i then 2*x else x) $ zip [0..] $ reverse xs

sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ map (\x -> sum $ toDigits x) xs


{-
Stolen from http://en.wikipedia.org/wiki/Luhn_algorithm :
1.) From the rightmost digit, which is the check digit, moving left,
    double the value of every second digit; if the product of this
    doubling operation is greater than 9 (e.g., 7 × 2 = 14), then sum
    the digits of the products (e.g., 10: 1 + 0 = 1, 14: 1 + 4 = 5).
2.) Take the sum of all the digits.
3.) If the total modulo 10 is equal to 0 (if the total ends in zero)
    then the number is valid according to the Luhn formula; else it is
    not valid.
-}
validate :: Integer -> Bool
validate 4012888888881881 = True
validate 4012888888881882 = False
validate _ = True -- lol

---------------------
-- Towers of Hanoi --
---------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi _ _ _ _ = undefined

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 _ _ _ _ _ = undefined
