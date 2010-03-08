import Timer

main = do
    time $ (fst.head) (dropWhile (\(i, f) -> f < 10^999) fibNums)

fibs = 0:1:zipWith (+) fibs (tail fibs)

fibNums = zip [0..] fibs
