import System(getArgs)
import Data.Char
import Timer

main = do
    args <- getArgs
    time $ length $ foldl (\x y -> x ++ (filter (not.isSpace) (english y))) "" [1..1000]

english 1000 = "one thousand"
english n = english' hnds tens unts
    where hndsDivMod = n `divMod` 100
          hnds = fst hndsDivMod
          tensDivMod = (snd hndsDivMod) `divMod` 10
          tens = fst tensDivMod
          unts = snd tensDivMod

english' 0 0 0 = "zero"
english' 0 0 u = units !! u
english' 0 1 u = teens !! u
english' 0 t 0 = tens !! t
english' 0 t u = (english' 0 t 0) ++ (english' 0 0 u)
english' h 0 0 = (english' 0 0 h) ++ " hundred"
english' h t u = (english' h 0 0) ++ " and " ++ (english' 0 t u)

units = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
tens = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
teens = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

