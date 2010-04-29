import Timer
import Maths
import Data.List
import Data.Maybe

-- We reduce the problem space by noting that:
-- the smallest prime in the 8-series must have 0 1 or 2 as the replaceable digit - anything greater and we don't have enough permutations to fit into the 8-series.
-- there must be at least three replaceable digits, fewer than that results in too many in the permutations being divisible by 3 and thus not prime. This can be confirmed by deducing all the digit-sums of all the 10 possibilities for 1 replaceable digit and 2 replaceable digits and adding them manually to the range [0..9] and seeing that for each value in the range we never get more than 7 digitsums that are not divisible by 3. There is probably a lessbrutish way of proving this but I couldn't work one out.

main = do
    time result

result = head $ first8Series $ filterPrimes $ map expand $ mapMaybe replaceableDigit $ filter (>1000) primes

first8Series = head . filter ((==8).length)
filterPrimes :: [[String]] -> [[Integer]]
filterPrimes = map ((filter isPrime).(map read))

expand replaceable = [map (\x -> if x == dgt then rpl else x) $ snd replaceable | rpl <- [dgt..'9']]
    where dgt = fst replaceable

replaceableDigit x
    | check '0' xStr = Just ('0', xStr)
    | check '1' xStr = Just ('1', xStr)
    | check '2' xStr = Just ('2', xStr)
    | otherwise = Nothing
    where xStr = show x
          check digit = (==3).length .(filter (==digit))
