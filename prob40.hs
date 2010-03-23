import Timer
import Utils

main = do
    time $  product $ map (\x -> digits !! x) [(10^x - 1) | x <- [0..6]]
        where digits = concat $ map toDigits [1..]
