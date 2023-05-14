periodo::Int
periodo = 7


vendas::Int->Int
vendas 0 = 0
vendas 1 = 41
vendas 2 = 12
vendas 3 = 48
vendas 4 = 21
vendas 5 = 91
vendas 6 = 54
vendas 7 = 30



diaCertaVenda::Int->Int->Int
diaCertaVenda _ 0 = vendas 0
diaCertaVenda x y
  |vendas y == x = y
  |otherwise = diaCertaVenda x (y - 1)


qVenda::Int->Int
qVenda x = diaCertaVenda x periodo  


totalVendas::Int->Int
totalVendas 0 = 0
totalVendas x = totalVendas(x-1) + vendas x 



qtVendasMaior::Int->Int->Int
qtVendasMaior _ 0 = 0
qtVendasMaior x y
  |vendas y > x = qtVendasMaior x (y-1) + 1 
  |otherwise = qtVendasMaior x (y-1)


totalVendasPar::Int->Int
totalVendasPar 0 = 0
totalVendasPar x
  |par (vendas x) = totalVendasPar(x-1) + vendas x
  |otherwise = totalVendasPar(x-1)


par::Int->Bool
par x = x `mod` 2 == 0


maiorVenda::Int->Int
maiorVenda 0 = 0
maiorVenda x
  |x < 0 || x > periodo = 0
  |vendas x > maiorVenda(x-1) = vendas x
  |otherwise = maiorVenda(x-1)


maiorDia::Int->Int
maiorDia 0 = 0
maiorDia x
  |vendas x == maiorVenda x = x
  |otherwise = maiorDia(x-1)


maior::Int->Int->Int
maior x y
  |x > y = x
  |otherwise = y

maiorVenda2::Int->Int
maiorVenda2 0 = 0
maiorVenda2 x
  |x<=periodo = maior (vendas x) (maiorVenda2(x-1))
  |otherwise = 0


--14 a
howManyLess::Int->Int->Int->Int
howManyLess 0 _ _ = 0
howManyLess x p1 p2
  |p2 < p1 || p2 > periodo = 0
  |vendas p2 < x = howManyLess x p1 (p2-1) + 1
  |otherwise = howManyLess x p1 (p2-1)


noZeroInPeriod::Int->Bool
noZeroInPeriod 0 = True
noZeroInPeriod x
  |vendas x == 0 = False
  |otherwise = noZeroInPeriod(x-1)


zeroList::Int->[Int]
zeroList 0 = []
zeroList x
  |vendas x == 0 = zeroList(x-1) ++ [x]
  |otherwise = zeroList(x-1)


vendasAbaixo::Int->Int->[Int]
vendasAbaixo _ 0 = []
vendasAbaixo x p
  |vendas p < x = vendasAbaixo x (p-1) ++ [p]
  |otherwise = vendasAbaixo x (p-1)


