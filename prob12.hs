import System(getArgs)
import List (group)
import Timer
import Maths

main = do
    args <- getArgs
    time $ fst $ head $ filter (\(x, y) -> y >= 500) trinumNumFactors

sqrti = floor.sqrt.fromIntegral

trinum n = n * (n + 1) `div` 2
trinums = map (trinum) [1..]

trinumNumFactors = map (\x -> (x, numFactors x)) trinums

pfactors n
    | pds == [] = [n]
    | otherwise = let (p, d) = head pds
                  in  p : pfactors d
    where pds = [(p, d) | p <- takeWhile (<= (sqrti n)) primes, let (d, r) = n `divMod` p, r == 0]

numFactors 1 = 1
numFactors n = product $ map ((+1).length) ((group . pfactors) n)
