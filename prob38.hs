import Timer
import Data.List

main = do
    time $ maximum $ map (read:: String -> Integer) $ filter isPandigital [concatProd n | n <- [1..9999]]

concatProd n = accumulate [show (n * i) | i <- [1..]] 
    where accumulate (x:y:xs)
            | (length x >= 9) = x
            | otherwise = accumulate ((x ++ y):xs)

isPandigital = (['1'..'9'] ==) . sort
