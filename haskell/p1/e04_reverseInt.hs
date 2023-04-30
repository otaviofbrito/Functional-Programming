reverseInt::Int->Int
reverseInt x
  |x > 0 =  mod x 10 *10 + reverseInt(div x 10)
  |otherwise = 0