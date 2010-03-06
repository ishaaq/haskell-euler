import System(getArgs)
import Data.Char
import Timer

main = do
    args <- getArgs
    time $ foldl (\x y -> x + (digitToInt y)) (0) (show (2^1000))
