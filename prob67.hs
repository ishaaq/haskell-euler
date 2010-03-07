import Timer
import System.IO

main = do
    timeIO $ processTriFile "prob67.txt"

trifold :: (Num a, Ord a) => [[a]] -> a
trifold rows = maximum (foldl1 (trizip) rows)
 where trizip topxs currxs = zipWith3 (\l r curr -> (max l r) + curr) (lshift topxs) (rshift topxs) currxs
       lshift (x:xs) = x:x:xs
       rshift xs = xs ++ [last xs]

splitLines :: (Read a) => String -> [[a]]
splitLines lns = map (map (read)) (map words (lines lns))

processTriFile :: (Num a, Read a, Ord a) => FilePath -> IO a
processTriFile filePath = do
    contents <- readFile filePath
    return (trifold (splitLines contents))
