mdc::Int->Int->Int
mdc m n
  |m <= 0 || n <= 0 = 0
  |m `mod` n /= 0 = mdc n (m `mod` n)
  |otherwise = n