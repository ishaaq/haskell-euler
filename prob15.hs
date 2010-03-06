import System(getArgs)
import Data.List
import Timer

main = do
    args <- getArgs
    time $ comb 40 20

comb n k = (fact n) `div` ((fact k) * (fact (n - k)))
    where fact = (facts !!)

facts = 0:1:(zipWith (*) (tail facts) [2..])
