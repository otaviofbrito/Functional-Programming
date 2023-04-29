totalSales::Int->Int
totalSales n 
  |n == 0 = sales 0
  |n > 0 = totalSales(n-1) + sales(n)
  |otherwise = 0

sales::Int->Int
sales x
  |x==0 = 12
  |x==1 = 0
  |x==2 = 30
  |x==3 = 25
  |otherwise = 0

maxi::Int->Int->Int
maxi m n
  |m >= n = m
  |otherwise = n

maxSales::Int->Int
maxSales n
  |n == 0 = sales 0 
  |otherwise = maxi (maxSales(n-1)) (sales n)

isZeroDay::Int->Bool
isZeroDay x = sales x == 0
 

zeroInPeriod::Int->Bool
zeroInPeriod 0 = isZeroDay 0
zeroInPeriod x = zeroInPeriod(x-1) || isZeroDay x


