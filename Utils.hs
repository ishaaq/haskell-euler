module Utils
(strToDigits, sumDigits) where

import Data.Char

sumDigits n = foldl (\x y -> x + (digitToInt y)) (0) (show n)

strToDigits = map (digitToInt)
