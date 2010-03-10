import Timer
import Data.List

main = do
    time $ fst (foldl divFold (0, 0) [1..1000])

divFold aVal@(a, ar) b = if br > ar then (b, br) else aVal
    where br = length (dCycle 1 b [])

dCycle n d nrs
    | r == 0 = [0]
    | otherwise = split d nr nrs
    where nr@(n', r) = (10 * n) `divMod` d

split d nr@(n, r) nrs
    | recur == [] = dCycle r d (nrs ++ [nr])
    | otherwise = map fst recur
    where (_, recur) = span (/= nr) nrs
