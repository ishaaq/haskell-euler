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
    print $ maximum $ filter (palstr.show) [x*y | x <-[100..999], y <- [100..999]]

palstr s = reverse s == s

