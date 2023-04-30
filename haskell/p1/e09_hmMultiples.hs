howManyMultiples::Int->Int->Int->Int
howManyMultiples x m n
  |n < m = 0
  |n `mod` x == 0 = howManyMultiples x m (n-1) + 1
  |otherwise = howManyMultiples x m (n-1)