{-# OPTIONS_GHC -fwarn-tabs #-}

module HaskellIntro where

import Set

-- Load this file into GHCi (say, with `ghci HaskellIntro.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

isThisWorking :: String
isThisWorking = "Yes"

--
-- Problem 1
--

-- get last digit of integer value
lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

toDigits :: Integer -> [Integer]
toDigits n
    | n < 1 = []
    | otherwise = toDigits (dropLastDigit n) ++ [lastDigit n]

reverseList :: [Integer] -> [Integer]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther xs = (reverseList (doubleEveryOtherHelper (reverseList xs)))
    where
        doubleEveryOtherHelper :: [Integer] -> [Integer]
        doubleEveryOtherHelper [] = []
        doubleEveryOtherHelper [x] = [x]
        doubleEveryOtherHelper (x:y:ys) = x : 2*y : (doubleEveryOtherHelper ys)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sumDigitsHelper x + sumDigits xs
    where
        sumDigitsHelper :: Integer -> Integer
        sumDigitsHelper n
            | n < 10 = n
            | otherwise = sumDigitsHelper (lastDigit n) + sumDigitsHelper (dropLastDigit n)

validate :: Integer -> Bool
validate n
    | (sumDigits (doubleEveryOther (toDigits n))) `mod` 10 == 0 = True
    | otherwise = False

--
-- Problem 2
--

pow :: (a -> a) -> Int -> a -> a
pow = error "pow not yet defined"

g :: Integer -> Integer
g = error "g not yet defined"

h :: Integer -> Integer
h = error "h not yet defined"

d :: Int -> Integer -> Integer
d = error "d not yet defined"

--
-- Problem 3
--

powerSet = error "powerSet not yet defined"
