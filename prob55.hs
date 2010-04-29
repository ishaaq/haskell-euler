import Timer
import Utils

main = do
    time $ length $ filter isLychrel [1..9999]

rev = reverse.show
isPalindromeNum = isPalindrome.show

isLychrel x = check x 0
    where check curr itr
            | itr == 50 = True
            | isPalindromeNum revadd = False
            | otherwise = check revadd (itr + 1)
            where revadd = curr + (read.rev) curr
