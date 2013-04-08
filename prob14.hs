import System.Environment(getArgs)
import qualified Data.Map as M
import Data.List
import Timer

main = do
    args <- getArgs
    time $ fst $ maxCollatzSeq 999999

-- most of the complexity below is because of memoization, due to which this program needs
-- additional heap space to run


maxCollatzSeq n = foldl1' max' $ collatzSeqLens n
       where max' (a, aSeqLen) (b, bSeqLen)
                | bSeqLen > aSeqLen = (b, bSeqLen)
                | otherwise = (a, aSeqLen)


collatzSeqLens n = M.toList $ M.filterWithKey (\k _ -> k <= n) (foldl' collatzM (M.singleton 1 0) [1..n])

collatzM mp n
    | M.member n mp = mp
collatzM mp n = M.insert n (1 + (collatzMNext M.! next)) collatzMNext
    where next = collatzn n
          collatzMNext = (collatzM mp next)

collatzn n
    | even n = n `div` 2
    | otherwise = 3 * n + 1

