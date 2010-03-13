import Timer
import Utils

main = do
    time $ sum (powerSumEquals 5)

powerSumEquals p = filter (isPowerSumEqual p) [1..(10^(maxDigitsForPower p) - 1)]

maxDigitsForPower p = length $ takeWhile (\(nines, ninesum) -> ninesum >= nines) $ powersums p

powersums p = map (\x -> (10^x - 1, x * power9)) [0..]
    where power9 = 9^p

digitsPowerSum p n = sum $ map (^p) $ toDigits n

isPowerSumEqual p n = n == (digitsPowerSum p n) && n /= 1
