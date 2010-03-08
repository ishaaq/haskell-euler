import List (nub)
import Timer
import Maths
import qualified Data.Map as M

main = do
    time $ M.foldWithKey (\k _ s -> k + s) 0 $ mapAmicables 10000

mapAmicables :: Int -> M.Map Int Bool
mapAmicables n = M.filter (== True) $ foldl (\x m -> amicableM sumFacts m x) M.empty xs
    where xs = [1..n]
          sumFacts = map (sum.properFactors) [1..n]

amicableM :: [Int] -> Int -> M.Map Int Bool -> M.Map Int Bool
amicableM sumFacts a = M.insert a (amicable a sumFacts)

amicable :: Int -> [Int] -> Bool
amicable a sumFacts = b /= a && (b <= (length sumFacts)) && sumFacts !! (b - 1) == a
    where b = sumFacts !! (a - 1)
