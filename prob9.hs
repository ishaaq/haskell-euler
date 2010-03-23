import Timer
import Maths

main = do
    time $ map (\(a, b, c) -> a * b *c) $ primitivePythtriplets 1000
