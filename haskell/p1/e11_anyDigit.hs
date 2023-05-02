anyDigit::Int->Int->Int
anyDigit p x 
  |x > 9 && p == 0 =  anyDigit p (div x 10) 
  | x > 9 && p /= 0 = anyDigit p (div x (10^p)) `mod` 10^p

