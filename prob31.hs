import Timer

main = do
    time $ ways [1,2,5,10,20,50,100,200] !! 200

ways [] = 1 : repeat 0
ways (coin:coins) = n 
          where n = zipWith (+) (ways coins) (replicate coin 0 ++ n)
