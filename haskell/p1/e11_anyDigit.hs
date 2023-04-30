anyDigit::Int->Int->Int
anyDigit p x 
  |x > 9 && p == 0 =  anyDigit p (div x 10) 
  |otherwise = anyDigit p (div x 10) `mod` 10