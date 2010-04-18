import Timer
import Maths

main = do
    time $ snd (foldl (\old@(l', ab') new@(l, ab) -> if (l > l') then new else old) (0, 0) coeffsPrimeLengths)

numPrimes a b = length (takeWhile (== True)  [isPrime (n^2 + a * n + b) | n <- [0..]])

coeffsPrimeLengths = [ (l, a * b) | a <- range, b <- range, let l = numPrimes a b] 
    where range = [-999..999]
