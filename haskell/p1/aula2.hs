inverte::[Int]->[Int]
inverte [] = []
inverte (x:y) = inverte y ++ [x]


listaImpar::[Int]->[Int]
listaImpar [] = []
listaImpar (x:y)
  |x `mod` 2 /= 0 = [x] ++  listaImpar y
  |otherwise = listaImpar y

periodo::Int
periodo = 7



vendas::Int->Int
vendas 0 = 0
vendas 1 = 41
vendas 2 = 72
vendas 3 = 48
vendas 4 = 0
vendas 5 = 91
vendas 6 = 55
vendas 7 = 30


listaVendas::Int->[Int]
listaVendas 0 = []
listaVendas x = listaVendas(x-1) ++ [vendas x]

listaDiaVendas::Int->[[Int]]
listaDiaVendas 0 = []
listaDiaVendas x = (x:[vendas x]) : listaDiaVendas(x-1)


maiorDaLista::[Int]->Int
maiorDaLista [] = 0
maiorDaLista (x:y)
  |x > maiorDaLista(y) = x
  |otherwise = maiorDaLista(y)

insereOrdenado::Int->[Int]->[Int]
insereOrdenado x [] = [x]
insereOrdenado x (a:b)
  |x < a = x:a:b
  |otherwise = [a] ++ insereOrdenado x b 


ordenaLista::[Int]->[Int]
ordenaLista [] = []
ordenaLista (a:b) = insereOrdenado a (ordenaLista b)


insereListaOrdenado::[Int]->[[Int]]->[[Int]]
insereListaOrdenado x [] = []
insereListaOrdenado x (a:b)
  |x<a = x:a:b
  |otherwise = [a] ++ insereListaOrdenado x b