import System(getArgs)
import Array
import Timer

main = do
    args <- getArgs
    time $ maximum $ filter (\x -> 600851475143 `mod` x == 0) $ (primesToN . floor . sqrt) 600851475143

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
