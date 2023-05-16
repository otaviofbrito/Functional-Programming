import Data.Char

somaTupla::(Int,Int)->Int
somaTupla (x,y) = x + y

prim::(Int,Int)->Int
prim (x,y) = x

seg::(Int,Int)->Int
seg (x,y) = y

somaTupla2::(Int,Int)->Int
somaTupla2 x = prim x + seg x


shift::((Int,Int),Int)->(Int,(Int,Int))
shift ((a,b), c) = (a,(b,c))

--

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



tuplaVendas::Int->[(Int,Int)]
tuplaVendas 0 = []
tuplaVendas x = tuplaVendas(x-1) ++ [(x,vendas x)]


totalVendasTupla::[(Int,Int)]->Int
totalVendasTupla [] = 0
totalVendasTupla ((x,y):b) = y + totalVendasTupla b 


vendasTupla::[(Int,Int)]->[Int]
vendasTupla [] = []
vendasTupla ((_,x):b) = x:vendasTupla b 

diasTupla::[(Int,Int)]->[Int]
diasTupla [] = []
diasTupla ((x,_):b) = x:diasTupla b 


listaTupla::[Int]->[Int]->[(Int,Int)]
listaTupla [] _ = []
listaTupla _ [] = []
listaTupla (a:b) (c:d) = (a,c):listaTupla b d



filtraNum::[Char]->[Char]
filtraNum [] = []
filtraNum (x:y)
  |isDigit x = filtraNum y
  |otherwise = x:filtraNum y

filtraChar::[Char]->[Char]
filtraChar [] = []
filtraChar (x:y)
  |isDigit x = x:filtraChar y
  |otherwise = filtraChar y


filtraLista::[(Bool,String)]->[String]
filtraLista [] = []
filtraLista ((b,s):c)
  |b = filtraNum s : filtraLista c
  |otherwise = filtraChar s : filtraLista c



max2::Int->Int->Int
max2 a b
  |a>b = a
  |otherwise = b

min2::Int->Int->Int
min2 a b
  |a<b = a
  |otherwise = b


min3max::Int->Int->Int->(Int,Int)
min3max a b c = (max2 c (max2 a b), min2 c (min2 a b))
