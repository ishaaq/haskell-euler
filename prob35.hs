import Timer
import Utils
import Maths
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List

main = do
    time $ length (circular 999999)

circular n = M.foldWithKey (\n isP acc -> if isP then n:acc else acc) [] (cycleMap n)

cycleMap n = foldl' (\mp n -> insrt n mp) M.empty [2..n]

cycles digits = map (toNumber) (shift (length digits) digits)

insrt n mp
    | M.member n mp = mp

insrt n mp
    | hasEven = mp
    | hasFive = mp
    | otherwise = insrt' (cycles digits) mp
    where digits = toDigits n
          multiDigits = (length digits) > 1
          hasEven = multiDigits && any even digits
          hasFive = multiDigits && 5 `elem` digits

insrt' cycs mp = M.union mp (M.fromList circPairs)
    where isCircular = (dropWhile isPrime' cycs) == []
          circPairs = foldl' (\acc x -> (x, isCircular):acc) [] cycs

isPrime' n = S.member n pSet

pSet = S.fromList $ primesToN 999999

shift _ [] = []
shift _ [x] = [[x]]
shift 0 xs = []
shift n (x:xs) = [shifted] ++ (shift (n-1) shifted)
    where shifted = xs ++ [x]
