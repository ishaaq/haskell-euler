import Timer
import Data.List(nub)

main = do
    time $  length $ nub [a^b | a <- [2..100], b <- [2..100]]
