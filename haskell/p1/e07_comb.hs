combination::Int->Int->Int
combination m n
  |m >= n =  (fact m) `div` (fact (m - n) * fact n)
  |otherwise = 0

fact::Int->Int
fact x
  |x <= 0 = 1
  |otherwise = fact(x-1) * x