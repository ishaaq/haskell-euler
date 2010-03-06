import System(getArgs)
import Timer

main = do
    args <- getArgs
    time $ map (\(a, b, c) -> a * b *c) $ pythtriplets 1000

pythtriplets n = [(a, b, c)|
                    a <- [1..(((n-1) `div` 3)-1)],
                    b <- [2..(n `div` 2 - 1)],
                    c <- [(((n-1) `div` 3) + 2)..(n-3)],
                    a^2 + b^2 == c^2,
                    a + b + c == n]
