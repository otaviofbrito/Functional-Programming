
sales::Int->Int
sales x
  |x==0 = 12
  |x==1 = 20
  |x==2 = 30
  |x==3 = 25
  |otherwise = 0
totalSales 0 = sales 0
totalSales n = totalSales(n-1) + (sales n)