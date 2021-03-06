module Utils
(csvToWords, strToDigits, sumDigits, toDigits, toNumber, maxWithIndex, charNum, isPalindrome) where

import Data.Char
import Data.List
import Data.Ord

sumDigits = sum.toDigits

strToDigits = map digitToInt

toDigits :: (Integral a, Show a) => a -> [Int]
toDigits = (map digitToInt).show

toNumber xs = toNumber' 0 xs

toNumber' acc [] = acc
toNumber' acc (x:xs) = toNumber' (acc + (10^(length xs) * x)) xs

maxWithIndex xs = maximumBy (comparing fst) $ zip xs [1..]

charNum =  (flip (-) (ord 'A' - 1)) . (ord)

csvToWords = words . (filter (not.isPunctuation)) . map (\x -> if x == ',' then ' ' else x)

isPalindrome xs = xs == (reverse xs)
