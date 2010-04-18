import Timer
import Data.Ratio

main = do
    time $ denominator . product $ ratioList

ratioList = [ x % z |
    x <- range,
    y <- range,
    z <- range,
    y /= z,
    9*x*z + y*z == 10*x*y]
    where range = [1..9]
    
