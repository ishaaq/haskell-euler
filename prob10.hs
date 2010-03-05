import System(getArgs)
import System.CPUTime
import Text.Printf
import Array

time f = do
    t0 <- getCPUTime
    print f
    t1 <- getCPUTime
    printf "%.6fs\n" (fromIntegral(t1-t0)/1000000000000 :: Double)

main = do
    args <- getArgs
    time $ sum.primesToN $ 2000000

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
