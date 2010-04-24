import Timer
import Maths

main = do
    time $ head $ filter (not.goldbachCheck) oddComposites

oddComposites = filter (not.isPrime) [3,5..]

goldbachCheck n = any isPrime [ n - 2*(m^2)| m <- [1..(sqrti (n `div` 2))]]
