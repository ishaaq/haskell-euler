import Timer
import Maths

main = do
    time $ diagsum 1001

diagsum n = ldiagsum n + rdiagsum n - 1

rdiagsum 1 = 1
rdiagsum n = rdiagsum (n - 2) + 2 * (n^2  - n +  1)

ldiagsum 1 = 1
ldiagsum n = ldiagsum (n - 2) + 2*(n^2 - 2 * n + 2)


