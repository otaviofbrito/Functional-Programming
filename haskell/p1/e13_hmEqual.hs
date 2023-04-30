howManyEqual::Int->Int->Int->Int
howManyEqual x y z
  |x /= y && x /= z && y/=z = 0
  |x == y && x == z = 3
  |otherwise = 2