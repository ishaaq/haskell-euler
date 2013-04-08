import System.Environment(getArgs)
import Timer

main = do
    time $ (sum [1..100])^2 - sum (map (^2) [1..100])  
