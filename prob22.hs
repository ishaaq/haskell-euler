import Timer
import System.IO
import Data.Char
import Data.List

main = do
    timeIO $ processFile "prob22.txt"

sortedNames rawContents =
    sort (words (filter (not.isPunctuation) (map (\x -> if x == ',' then ' ' else x) rawContents)))

sumScores :: [String] -> Int
sumScores names = sum (zipWith (*) (map (alphScore) names) [1..])

alphScore name = sum (map charNum name)

charNum c = 1 + ord c - ord 'A'

processFile :: FilePath -> IO Int
processFile filePath = do
    contents <- readFile filePath
    let names = sortedNames contents
    return (sumScores names)
