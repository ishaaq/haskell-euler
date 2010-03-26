import Timer
import Utils
import Maths
import Data.List

main = do
    time $ sum $ filter isPandigitalProd [1..10^6-1]


isPandigitalProd n = (not.null) [(m1, m2) | not.(elem 0) $ prodDigits, uniqDigits prodDigits,
                            m1 <- properFactors n, 
                            let m2 = n `div` m1, 
                            isPandigital ((toDigits m1) ++ (toDigits m2) ++ (toDigits n))]
        where prodDigits = toDigits n
              uniqDigits digits = (maximum $ map (length) ((group.sort) digits)) == 1
                            
isPandigital digits = (length digits) == 9 
                            && (length $ nub digits) == 9 
                            && (dropWhile (/= 0) digits) == []
