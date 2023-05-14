--1
f1::Double->Double
f1 x
  |x >= 0 = (x+4)/(x+2)
  |otherwise = 2/x


f2::Double->Double->Double
f2 x y
 |x >= y = x+y
 |otherwise = x-y

f3::Double->Double->Double->Double
f3 x y z
  |x+y>z = x+y+z
  |x+y<z = x-y-z
  |otherwise = 0

--2
fat::Int->Int
fat x
  |x <= 0 = 1 
  |otherwise = x * fat(x-1)

--3
soma::Int->Int->Int
soma a b = a+b;

mult::Int->Int->Int
mult a 1 = a
mult a b = soma a (mult a (b-1))


--4
reverseInt::Int->Int
reverseInt x
  |x > 0 =  mod x 10 *10 + reverseInt(div x 10)
  |otherwise = 0


--5
fourPower::Int->Int
fourPower x = square x * square x

square::Int->Int
square x = x*x


--6
seqSr6::Int->Double
seqSr6 i
  |i == 0 = sqrt 6 
  |i > 0 =  6 + seqSr6(i - 1)
  |otherwise = 0


--7
--Combinatory version
combination::Int->Int->Int
combination m n
  |m >= n =  (fact m) `div` (fact (m - n) * fact n)
  |otherwise = 0

fact::Int->Int
fact x
  |x <= 0 = 1
  |otherwise = fact(x-1) * x


--8
mdc::Int->Int->Int
mdc m n
  |m <= 0 || n <= 0 = 0
  |m `mod` n /= 0 = mdc n (m `mod` n)
  |otherwise = n


--9
howManyMultiples::Int->Int->Int->Int
howManyMultiples x m n
  |n < m = 0
  |n `mod` x == 0 = howManyMultiples x m (n-1) + 1
  |otherwise = howManyMultiples x m (n-1)


--10
lastDigit::Int->Int
lastDigit x = mod x 10


--11
anyDigit::Int->Int->Int
anyDigit p x 
  |x > 9 && p == 0 =  anyDigit p (div x 10) 
  | x > 9 && p /= 0 = anyDigit p (div x (10^p)) `mod` 10^p


--12
allDifferent::Int->Int->Int->Bool
allDifferent m n p = (m/=n) && (n/=p) && (m/=p)


--13
howManyEqual::Int->Int->Int->Int
howManyEqual x y z
  |x /= y && x /= z && y/=z = 0
  |x == y && x == z = 3
  |otherwise = 2

--16

funny::Int->Int->Int->Bool
funny x y z = x > z || not(y >= x) && x < z



funny2::Int->Int->Int->Bool
funny2 x y z  
  |x > z = True
  |y >= x = False
  |otherwise = True


--19
duplicarString::String->Int->String
duplicarString _ 0 = []
duplicarString x y = x ++ duplicarString x (y-1)


--20
pushRight::String->Int->String
pushRight s p
  |p <= length s = s
  |otherwise = ">" ++ pushRight s (p-1)


--21
infix 8 &-

(&-)::Int->Int->Int
x &- y = x - (2*y)


--22

inverteListaINT::[Int]->[Int]
inverteListaINT [] = []
inverteListaINT (a:b) = inverteListaINT b ++ [a]


--23

separa::[Int]->([Int],[Int])
separa [] = ([],[])
separa (a:b)
  |a `mod` 2 == 0 = (a:fst(separa b), snd(separa b))
  |otherwise = (fst(separa b) ,a:snd(separa b))


--24

