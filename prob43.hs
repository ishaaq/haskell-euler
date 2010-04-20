import Timer
import Utils
import Maths
import Data.List

main = do
    time $ sum divisiblePandigitals

divisiblePandigitals = map (fst) $ filter (isDivisible.snd) $ map (split) $ permutations "0123456789"

isDivisible =  (notElem False) . (zipWith (\prm splt -> splt `mod` prm == 0) primes)

split :: String -> (Integer, [Integer])
split p = (read p, split' (drop 1 p))
    where split' p
            | (length p) < 3 = []
            | otherwise = (read (take 3 p)) : (split' (drop 1 p))
