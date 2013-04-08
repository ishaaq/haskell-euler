import System.Environment(getArgs)
import Timer

main = do
    time $ head $ filter(divby [20,19..2]) [1..]

divby [] _ = True
divby (x:xs) y = y `mod` x == 0 && divby xs y
