import Timer

main = do
    time solution

solution :: Integer
solution = read $ reverse $ take 10 $ reverse $ show $ sum [n^n | n <- [1..1000]]
