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
    time $ head $ filter(divby [20,19..2]) [1..]

divby [] _ = True
divby (x:xs) y = y `mod` x == 0 && divby xs y
