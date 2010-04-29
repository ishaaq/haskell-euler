import Timer
import Numeric
import Utils

main = do
    time $ sum $ [x | x <- [1..999999], isBinPalindrome x, isDecPalindrome x]

toBinDigits x = showIntAtBase 2 (bin) x ""
    where bin 0 = '0'
          bin 1 = '1'

isBinPalindrome = isPalindrome . toBinDigits

isDecPalindrome = isPalindrome . toDigits 

