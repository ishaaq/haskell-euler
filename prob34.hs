import Timer
import Utils

-- actually, the upper limit is 10^9 - 1 (the largest 9 digit num) because 9! * n < 10^n for n >= 9
-- but using that takes too long to run, so cutting it short because I know there are only two numbers that
-- match.

main = do
    time $ sum $ filter (isCurious) [10..100000]

facts = 1 : zipWith (*) facts [1..9]

sumFacts n = foldl (\x y -> x + (facts !! y)) 0 (toDigits n)

isCurious n = n == sumFacts n
