import Timer

main = do
    time $ head ds

pents = [n*(3*n-1) `div` 2| n <-[1..]]

ds = [x-y | x <- pents, y <- takeWhile (<x) pents, isPent (x-y), isPent (x+y)]

isPent n = (head (dropWhile (<n) pents)) == n
