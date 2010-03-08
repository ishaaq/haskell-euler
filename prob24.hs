import Timer
import Data.List

main = do
    time $ (read ((sort (permutations "0123456789")) !! 999999) :: Integer)
