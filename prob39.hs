import Timer
import Maths
import Utils

main = do
    time $ snd $ maxWithIndex (numTriplets 1000)

numTriplets l = [ sum [numPTriplets !! (n - 1)| f <- [1..n - 1], n `mod` f == 0] | n <- range]
    where range = [1..l]
          numPTriplets = map (length.primitivePythtriplets) range
