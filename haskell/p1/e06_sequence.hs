seqSr6::Int->Double
seqSr6 i
  |i == 0 = sqrt 6 
  |i > 0 =  6 + seqSr6(i - 1)
  |otherwise = 0