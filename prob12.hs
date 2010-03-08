import List (group)
import Timer
import Maths

main = do
    time $ fst $ head $ filter (\(x, y) -> y >= 500) trinumNumFactors

trinum n = n * (n + 1) `div` 2
trinums = map (trinum) [1..]

trinumNumFactors = map (\x -> (x, numFactors x)) trinums

numFactors 1 = 1
numFactors n = product $ map ((+1).length) ((group . primeFactors) n)
