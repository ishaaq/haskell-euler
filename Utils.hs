module Utils
(csvToWords, strToDigits, sumDigits, toDigits, toNumber, maxWithIndex, charNum, isPalindrome) where

import Data.Char
import Data.List
import Data.Ord

sumDigits n = foldl (\x y -> x + (digitToInt y)) (0) (show n)

strToDigits = map (digitToInt)

toDigits n = reverse $ toDigits' n

toDigits' n
    | d == 0 = [r]
    | otherwise = r:toDigits' d
    where (d, r) = n `divMod` 10

toNumber xs = toNumber' 0 xs

toNumber' acc [] = acc
toNumber' acc (x:xs) = toNumber' (acc + (10^(length xs) * x)) xs

maxWithIndex xs = maximumBy (comparing fst) $ zip xs [1..]

charNum =  (flip (-) (ord 'A' - 1)) . (ord)

csvToWords = words . (filter (not.isPunctuation)) . map (\x -> if x == ',' then ' ' else x)

isPalindrome xs = xs == (reverse xs)
