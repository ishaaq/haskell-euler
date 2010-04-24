module Maths
(isPrime, primes, primesToN, factorial, primeFactors,
 properFactors, numCombinations, combinations,
 numAllCombinations, primitivePythtriplets, sqrti) where

import Array
import Data.List

primes :: (Integral a) => [a]
primes = 2 : filter isPrime [3,5..]

isPrime a = isPrime' a primes
isPrime' a (p:ps)
    | a == 1         = False
    | p*p > a        = True
    | a `mod` p == 0 = False
    | otherwise      = isPrime' a ps

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

sqrti :: (Integral a) => a -> a
sqrti = floor.sqrt.fromIntegral

properFactors :: Int -> [Int]
properFactors 1 = [1]
properFactors n = (filter (/= n) $ nub (map product ((subsequences.primeFactors) n)))

numCombinations n k = (factorial n) `div` ((factorial k) * (factorial (n - k)))

combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- combinations (n-1) xs']

numAllCombinations n = 2^n - 1

primitivePythtriplets l = [(a,b,c) | n <- [1..limit],
                        m <- [n+1..limit],
                        let msq = m^2, let nsq = n^2,
                        let a = msq - nsq, 
                        let b = 2*m*n, 
                        let c = msq + nsq,
                        a+b+c==l]
    where limit = floor . sqrt . fromIntegral $ l
