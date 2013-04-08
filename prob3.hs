import System.Environment(getArgs)
import Maths
import Timer

main = do
    args <- getArgs
    time $ maximum $ filter (\x -> 600851475143 `mod` x == 0) $ (primesToN . floor . sqrt) 600851475143
