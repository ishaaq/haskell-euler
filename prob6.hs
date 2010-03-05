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
    time $ (sum [1..100])^2 - sum (map (^2) [1..100])  
