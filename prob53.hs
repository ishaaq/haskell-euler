import Timer
import Maths
import Data.List

main = do
    time $ length numCombs

numCombs = filter (>1000000) [numCombinations n r | n <- [1..100], r <- [0..n]]
