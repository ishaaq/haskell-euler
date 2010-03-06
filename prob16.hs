import System(getArgs)
import System.CPUTime
import Text.Printf
import Data.Char

time f = do
    t0 <- getCPUTime
    print f
    t1 <- getCPUTime
    printf "%.6fs\n" (fromIntegral(t1-t0)/1000000000000 :: Double)

main = do
    args <- getArgs
    time $ foldl (\x y -> x + (digitToInt y)) (0) (show (2^1000))
