antFib::Int->Int
antFib 0 = 0;
antFib 1 = 0;
antFib 2 = 1;
antFib n
  |n > 2 = antFib(n-2) + antFib(n-3)
  |otherwise = -1
