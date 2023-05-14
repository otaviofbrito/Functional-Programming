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
maiorDaLista [x] = x
maiorDaLista (x:y)
  |x > maiorDaLista(y) = x
  |otherwise = maiorDaLista(y)

insereOrdenado::Int->[Int]->[Int]
insereOrdenado x [] = [x]
insereOrdenado x (a:b)
  |x <= a = x:(a:b)
  |otherwise = [a] ++ insereOrdenado x b 


ordenaLista::[Int]->[Int]
ordenaLista [] = []
ordenaLista (a:b) = insereOrdenado a (ordenaLista b)


insereListaOrdenado::[Int]->[[Int]]->[[Int]]
insereListaOrdenado x [] = [x]
insereListaOrdenado x (a:b)
  |x <= a = x:(a:b)
  |otherwise = [a] ++ insereListaOrdenado x b

ordenaListaLista::[[Int]]->[[Int]]
ordenaListaLista [] = []
ordenaListaLista (a:b) = insereListaOrdenado a (ordenaListaLista b)



listaDobro::[Int]->[Int]
listaDobro [] = []
listaDobro (x:y) = [x*2] ++ listaDobro y


membro::Int->[Int]->Bool
membro _ [] = False
membro x (a:b)
  |x == a = True
  |otherwise = membro x b

membro2::Int->[Int]->Bool
membro2 _ [] = False
membro2 x (a:b) = (a==x) || membro x b


isDigit::Char->Bool
isDigit c = c >= '0' && c <= '9' 

digitos::String->String
digitos [] = []
digitos (a:b)
  |isDigit a = [a] ++ digitos b
  |otherwise = digitos b


