module Utils
(strToDigits, sumDigits, toDigits, toNumber, maxWithIndex) where

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
