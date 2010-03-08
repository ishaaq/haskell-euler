import Timer
import System.IO
import Maths
import qualified Data.Set as S

main = do
    time $ S.fold (+) 0 nonAbundantSums

maxVal = 28123
range = [1..maxVal]
abundants = filter isAbundant range
abundantPairSums = S.fromList [z | x <- abundants, y <- abundants, let z = x + y, z <= maxVal]
nonAbundantSums = (S.fromList range) S.\\ abundantPairSums

isAbundant n = ((sum factors) > n)
    where factors = properFactors n
