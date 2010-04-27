import Timer
import Maths
import Data.List
import Data.Maybe

main = do
    time $ maxPrimeSumSequence primes'

primes' = takeWhile (<1000000) primes

maxPrimeSumSequence = fst.(foldl' checkMax (0, 0)).maxPrimeSums
    where checkMax s1@(sum1, seqSize1) s2@(sum2, seqSize2)
            | seqSize2 < seqSize1 = s1
            | otherwise = s2

maxPrimeSums = catMaybes . maxPrimeSums'
    where maxPrimeSums' xs
            | null xs = []
            | otherwise = maxPrimeSum xs : maxPrimeSums' (tail xs)

maxPrimeSum ps 
    | null primeSums = Nothing
    | otherwise = Just (last primeSums)
    where sum' = (head ps) : zipWith (+) sum' (tail ps)
          primeSums = filter (isPrime.fst) $ takeWhile ((<1000000).fst) $ zip sum' [1..] 
