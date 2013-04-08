import System.Environment(getArgs)
import Timer

main = do
    print $ maximum $ filter (palstr.show) [x*y | x <-[100..999], y <- [100..999]]

palstr s = reverse s == s

