import Timer
import Utils
import Data.List

main = do
    timeIO $ processFile "prob22.txt"

sumScores :: [String] -> Int
sumScores names = sum (zipWith (*) (map (alphScore) names) [1..])

alphScore name = sum (map charNum name)

processFile :: FilePath -> IO Int
processFile filePath = do
    fmap (sumScores . sort . csvToWords) (readFile filePath)
