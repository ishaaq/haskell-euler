module Utils
(strToDigits, sumDigits, toDigits) where

import Data.Char

sumDigits n = foldl (\x y -> x + (digitToInt y)) (0) (show n)

strToDigits = map (digitToInt)

toDigits n = reverse $ toDigits' n

toDigits' n
    | d == 0 = [r]
    | otherwise = r:toDigits' d
    where (d, r) = n `divMod` 10
