import System(getArgs)
import Maths
import Timer

main = do
    args <- getArgs
    time $ sum.primesToN $ 2000000
