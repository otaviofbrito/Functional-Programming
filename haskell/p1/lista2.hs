import Data.Char

--1

dist = ord 'A' - ord 'a'


convertChar::Char->Char
convertChar x
  |isLower x = chr ((ord x) + dist)                
  |otherwise = chr ((ord x) - dist)


converte2::Char->(Char,Char,Int)
converte2 x = (x,convertChar x, ord x)


--2

pessoa::Int->(String,Int,Char)
pessoa rg
  |rg == 1 = ("Joao Silva", 150, 'm')
  |rg == 2 = ("Jonas Souza", 51, 'm')
  |rg == 3 = ("Jocileide Strauss" , 21, 'f')
  |rg == 4 = ("JMDODM", 2, 'm')
  |otherwise = ("Nao ha ninguem mais", 9999, 'x')


nome::(String,Int,Char)->String
nome (a,_,_) = a

idade::(String,Int,Char)->Int
idade (_,b,_) = b

sexo::(String,Int,Char)->Char
sexo (_,_,c) = c

--a

menorIdade::Int->Int
menorIdade 0 = idade (pessoa 0)
menorIdade x
  |idade (pessoa x) <  menorIdade (x-1) = idade(pessoa x)
  |otherwise = menorIdade(x-1)


nomeMenorIdade::Int->String
nomeMenorIdade 0 = nome (pessoa 0)
nomeMenorIdade x
  |idade (pessoa x) == menorIdade x = nome (pessoa x)
  |otherwise = nomeMenorIdade (x-1)



masculino::Int->Int
masculino 0 = 0
masculino x
  |sexo (pessoa x) == 'm' = masculino (x-1) +1
  |otherwise = masculino (x-1)


maiorIdade::Int->Int
maiorIdade 0 = 0
maiorIdade x
  |idade (pessoa x) > maiorIdade(x-1) = idade(pessoa x)
  |otherwise = maiorIdade(x-1)

rgMaiorIdade::Int->Int
rgMaiorIdade 0 = 0
rgMaiorIdade x
  |idade(pessoa x) == maiorIdade x = x
  |otherwise = maiorIdade (x-1)


menor::Int->Int->Int
menor a b
  |a<b = a
  |otherwise =  b

maior::Int->Int->Int
maior a b
  |a>b = a
  |otherwise = b


--4
ordena::Int->Int->Int->Int->(Int,Int,Int,Int)
ordena a b c d
  |