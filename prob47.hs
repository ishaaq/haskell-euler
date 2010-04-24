import Timer
import Maths
import Data.List

main = do
    time $ (fst.head.(filter snd)) consecutives

consecutives = zip range $ zipWith4 (check) numFactors (drop 1 numFactors) (drop 2 numFactors) (drop 3 numFactors)
    where range = [1..]
          numFactors = map (length.nub.primeFactors) range
          check f1 f2 f3 f4 = f1 == 4 && f1 == f2 && f2 == f3 && f3 == f4
