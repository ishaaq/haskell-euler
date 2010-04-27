import Timer
import Maths
import Data.List

main = do
    time match

fourDigitPrimes = filter isPrime [1001..9999]

match :: Integer
match = head [read (aStr ++ bStr ++ cStr) | a <- fourDigitPrimes,
                                  a /= 1487,
                                  b <- dropWhile (<=a) fourDigitPrimes,
                                  let aStr = show a,
                                  let bStr = show b,
                                  let sortaStr = sort aStr,
                                  sortaStr == sort bStr,
                                  let c = 2*b - a,
                                  let cStr = show c,
                                  sortaStr == sort cStr,
                                  isPrime c]
