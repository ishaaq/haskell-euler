module Maths
(primes, primesToN, factorial, comb) where

import Array

primes :: [Integer]
primes = 2:3:primes'
  where
    1:p:candidates = [6*k+r | k <- [0..], r <- [1,5]]
    primes'        = p : filter isPrime candidates
    isPrime n      = all (not . divides n)
                       $ takeWhile (\p -> p*p <= n) primes'
    divides n p    = n `mod` p == 0

primesToN :: Integer -> [Integer]
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

comb n k = (factorial n) `div` ((factorial k) * (factorial (n - k)))
