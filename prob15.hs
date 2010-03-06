import System(getArgs)
import Data.List
import Timer
import Maths

main = do
    args <- getArgs
    time $ comb 40 20
