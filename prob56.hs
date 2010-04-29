import Timer
import Utils

main = do
    time $ maximum powerSums

powerSums = [sumDigits (a^b) | a<-range, b<-range]
    where range = [1..100]
