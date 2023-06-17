import Data.Char

multLista::Int->[Int]->[Int]
multLista _ [] = []
multLista x (a:b) = (x*a):multLista x b


listCom::[Int]->[Int]
listCom l = [a | a <- l]

par::Int->Bool
par x = x `mod` 2 == 0

listComPar::Int->[Int]->[Int]
listComPar x l = [x*a | a <- l, par a]


sToInt::String->[Int]
sToInt s = [ord a | a <- s, ord a >= 97 && ord a <= 122]


maiorD::(Int,Int)->Int
maiorD (x,y)
  |x>y=x
  |otherwise = y

listaMaiorDupla::[(Int,Int)] -> [Int]
listaMaiorDupla ld = [maiorD a | a <- ld]
 

listBool::[(Bool,Int)]->[Int]
listBool ld = [x | (b,x) <-ld, b]


filtraPar::(Bool,[Int])->[Int]
filtraPar (b,l)
  |b = [a | a<-l, not(par a)]
  |otherwise = l

filtraListaBool::[(Bool,[Int])]->[[Int]]
filtraListaBool ld = [filtraPar a| a <-ld]



