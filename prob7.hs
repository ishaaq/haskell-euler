import System.Environment(getArgs)
import Timer
import Maths

main = do
    args <- getArgs
    time $ primes !! 10000
