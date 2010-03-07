import Timer
import  Data.Time.Calendar.WeekDate
import  Data.Time.Calendar

-- note that it would be much simpler to use the Data.Time modules
-- but I think that would be missing the point of the excercise.

main = do
    time $ length $ filter (isSunday) [(y, m, 1) | y <- [1901..2000], m <- [1..12]]

isSunday date = getDay date == 0

-- this algorithm derived courtesy of Wikipedia:
-- http://en.wikipedia.org/wiki/Calculating_the_day_of_the_week#Finding_day_0_for_a_given_year

getDay (year, month, date) = mod (centNum + yearDigits + yearNum + monthNum + date) 7
    where yearParts = year `divMod` 100
          centNum = 2 * (3 - (mod (fst yearParts) 4))
          yearDigits = snd yearParts
          yearNum = yearDigits `div` 4
          monthNum
            | isLeap year && month == 1 = 6
            | isLeap year && month == 2 = 2
            | otherwise = monthsTable !! (month - 1)

monthsTable = [0, 3, 3, 6, 1, 4, 6, 2, 5, 0, 3, 5]

isLeap year = isDiv400 || (not isDiv100) && isDiv4
    where isDiv100 = isDiv 100 year
          isDiv4 = isDiv 4 year
          isDiv400 = isDiv 400 year

isDiv n x = (mod x n) == 0
