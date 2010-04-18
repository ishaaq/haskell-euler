import Timer
import Utils
import Data.Char

main = do
    timeIO $ processFile "prob42_words.txt"

triangleNums = [ x | n <- [1..], let x = n*(n+1) `div` 2]

isTriangleWord wrd = n == (head $ dropWhile (<n) triangleNums)
    where n = sum $ map (charNum) wrd

countTriangleWords = length . filter (isTriangleWord)

processFile :: FilePath -> IO Int
processFile filePath = do
    fmap (countTriangleWords . csvToWords) (readFile filePath)
