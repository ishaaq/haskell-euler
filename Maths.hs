module Maths
(primes, primesToN, factorial, primeFactors, numCombinations, combinations, numAllCombinations, allCombinations) where

import Array
import Data.List (tails, nub)

primes :: (Integral a) => [a]
primes = 2:3:primes'
  where
    1:p:candidates = [6*k+r | k <- [0..], r <- [1,5]]
    primes'        = p : filter isPrime candidates
    isPrime n      = all (not . divides n)
                       $ takeWhile (\p -> p*p <= n) primes'
    divides n p    = n `mod` p == 0

primesToN ::(Integral a, Enum a, Ix a) => a -> [a]
primesToN n = 2: [i | i<-[3,5..n], ar!i]
   where
    ar = f 5 $ accumArray (\a b->False) True (3,n) 
                        [(i,()) | i<- [9,15..n]]
    f p a | q > n = a 
          | True  = if null x then a' else f (head x) a'
      where q = p*p 
            a'= a//[(i,False)|i<-[q,q+2*p..n]]
            x = [i | i<-[p+2,p+4..n], a' !i]

facts = 0:1:(zipWith (*) (tail facts) [2..])
factorial = (facts !!)

primeFactors :: (Integral a) => a -> [a]
primeFactors n
    | pds == [] = [n]
    | otherwise = let (p, d) = head pds
                  in  p : primeFactors d
    where pds = [(p, d) | p <- takeWhile (<= (sqrti n)) primes, let (d, r) = n `divMod` p, r == 0]
          sqrti = floor.sqrt.fromIntegral

numCombinations n k = (factorial n) `div` ((factorial k) * (factorial (n - k)))

combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- combinations (n-1) xs']

numAllCombinations n = 2^n - 1
allCombinations :: [a] -> [[a]]
allCombinations xs = [x | k <- [1..(length xs)], x <- combinations k xs]
