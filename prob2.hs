import System(getArgs)
import System.CPUTime
import Text.Printf

time f = do
    t0 <- getCPUTime
    print f
    t1 <- getCPUTime
    printf "%.6fs\n" (fromIntegral(t1-t0)/1000000000000 :: Double)

fibs = 0:1:zipWith (+) fibs (tail fibs)

main = do
    args <- getArgs
    time $ foldl (+) 0 $ filter (\x -> (x `mod` 2) == 0) $ takeWhile (<4000000) fibs

