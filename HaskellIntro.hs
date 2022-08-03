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
pow _ 0 x = x
pow f n x = pow f (n-1) (f x)

-- hofstadter G
g :: Integer -> Integer
g 0 = 0
g n = n - (pow g 2 (n-1))

-- hofstadter H
h :: Integer -> Integer
h 0 = 0
h n = n - (pow h 3 (n-1))

-- family of D sequences to demonstrate partial application
d :: Int -> Integer -> Integer
d i 0 = 0
d i n = n - (pow (d i) i (n-1))

--
-- Problem 3
--

powerSet = error "powerSet not yet defined"
