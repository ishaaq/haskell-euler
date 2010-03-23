import Timer
import Utils
import Maths
import Data.List

main = do
    time $ sum $ take 11 truncatables

truncatables = [p | p <- primes, isTruncatable p]

isTruncatable :: (Integral a) => a -> Bool
isTruncatable p = (length digits > 1) && (all isPrime truncs)
    where digits = toDigits p
          generate f = map (toNumber) $ filter (/=[]) (f digits)
          truncs = nub $ (generate tails) ++ (generate inits)

isPrime p
    | ps' == [] = False
    | otherwise = head ps' == p
    where ps' = dropWhile (<p) primes