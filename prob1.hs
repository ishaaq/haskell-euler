import System(getArgs)
import System.CPUTime
import Text.Printf

time f = do
    t0 <- getCPUTime
    print f
    t1 <- getCPUTime
    printf "%.6fs\n" (fromIntegral(t1-t0)/1000000000000 :: Double)

main = do
    args <- getArgs
    time $ foldl (+) 0 (filter(modn [3,5]) [1..999])

modn [] y = False
modn (x:xs) y = (y `mod` x == 0) ||  (modn xs y)
