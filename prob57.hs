import Timer
import Utils
import Data.Ratio

main = do
    time $ length $ filter fltr $ take 1000 expansions

expansions = 1+(1%2) : [ (1 + 1/(1 + x)) | x <- expansions]

fltr ratio = (numDigits.numerator) ratio > (numDigits.denominator) ratio

numDigits = length . show
