import System(getArgs)
import System.CPUTime
import Text.Printf
import Data.List

time f = do
    t0 <- getCPUTime
    print f
    t1 <- getCPUTime
    printf "%.6fs\n" (fromIntegral(t1-t0)/1000000000000 :: Double)

main = do
    args <- getArgs
    time $ comb 40 20

comb n k = (fact n) `div` ((fact k) * (fact (n - k)))
    where fact = (facts !!)

facts = 0:1:(zipWith (*) (tail facts) [2..])
