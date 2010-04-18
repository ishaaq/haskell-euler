import Timer
import Maths
import Data.List

{- n-digit pandigitals with n=8, or n=9 cannot be prime
 - as sum[1..8] and sum[1..9] are both divisible by 9 and
 - so those pandigitals will all be divisible by 9 by
 - the 9-divisibility rule
 -}

main = do
    time $ head $ filter isPrime . filter isPandigital $ [7654321, 7654319..]

isPandigital n = isPrefixOf (sort (show n)) "1234567"
