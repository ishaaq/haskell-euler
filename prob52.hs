import Timer
import Data.List
import Control.Applicative

main = do
    time $ head matches

matches = filter (sameDigits.multiples) [1..]
    where sameDigits = (==1).length.nub.(map (sort.show))
          multiples = sequence (fmap (*) [2..6])
