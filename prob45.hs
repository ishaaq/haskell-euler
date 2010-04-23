import Timer

main = do
    time $ head $ dropWhile (<= 40755) (intersection tris pents hexs)

tris = [n*(n+1) `div` 2| n <-[1..]]
pents = [n*(3*n-1) `div` 2| n <-[1..]]
hexs = [n*(2*n-1) | n <-[1..]]

intersection :: [Integer] -> [Integer] -> [Integer] -> [Integer]
intersection ts'@(t:ts) ps'@(p:ps) hs'@(h:hs)
    | t == p && p == h = t:(intersection ts ps hs)
    | otherwise = let drp = dropWhile (< maximum [t, p, h])
        in intersection (drp ts') (drp ps') (drp hs')
