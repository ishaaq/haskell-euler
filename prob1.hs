import System(getArgs)
import Timer

main = do
    args <- getArgs
    time $ foldl (+) 0 (filter(modn [3,5]) [1..999])

modn [] y = False
modn (x:xs) y = (y `mod` x == 0) ||  (modn xs y)
