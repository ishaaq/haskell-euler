import System.Environment(getArgs)
import Timer

fibs = 0:1:zipWith (+) fibs (tail fibs)

main = do
    args <- getArgs
    time $ foldl (+) 0 $ filter (\x -> (x `mod` 2) == 0) $ takeWhile (<4000000) fibs

