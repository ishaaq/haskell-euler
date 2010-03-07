module Timer
(time, timeIO) where

import System.CPUTime
import Text.Printf

time :: (Show a) => a -> IO ()
time f = do
    timeIO (return f)

timeIO :: (Show a) => IO a -> IO ()
timeIO f = do
    t0 <- getCPUTime
    result <- f
    print result
    t1 <- getCPUTime
    printf "%.6fs\n" (fromIntegral(t1-t0)/1000000000000 :: Double)

