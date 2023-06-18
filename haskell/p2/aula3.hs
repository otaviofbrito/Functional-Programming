import Data.Char

type Compara = Int->Int->Int

--maior::Int->Int->Int
maior x y
  |x > y = x
  |otherwise = y

--menor::Int->Int->Int
menor x y
  |x > y = y
  |otherwise = x


operador::(t->t->z)->t->t->z
operador op a b = op a b  


checkPos::Int->Char->Bool
checkPos x c = x == ord c


comparaTupla::Compara->[(Int,Int)]->[Int]
comparaTupla f ld = [ f a b | (a,b) <- ld]

funcG::(t->t->t)->[(t,t)]->[t]
funcG f ld = [ f a b | (a,b) <- ld]

funcG2::(t->u->z)->[(t,u)]->[z]
funcG2 f ld = [ f a b | (a,b) <- ld]


myMap::(t->z)->[t]->[z]
myMap f l = [f x | x <- l]

multplica2 x = 2*x

opLista::(Int->Int)->[Int]->[Int]
opLista _ [] = []
opLista f (a:b) = (f a):opLista f b


soma1::Int->Int->Int
soma1 x y = x + y

maxi::Int->Int->Int
maxi x y
  |x >= y  = x
  |otherwise = y

fold::(Int->Int->Int)->[Int]->Int
fold _ [a] = a 
fold f (a:b:c) = f a (fold f (b:c))

isDigit1::Char->Bool
isDigit1 c = c >= '0' && c <= '9'


filterString::(Char->Bool)->String->String
filterString f s = [x | x <- s, f x]

filterString2::(Char->Bool)->String->String
filterString2 _ [] = []
filterString2 f (a:b)
  |f a = a:filterString2 f b
  |otherwise = filterString2 f b


listLen::[t]->Int
listLen [] = 0
listLen (a:b) = 1 + listLen b

rev::[t]->[t]
rev [] = []
rev (a:b) = rev b ++ [a]

myZip::[t]->[u]->[(t,u)]
myZip _ [] = []
myZip [] _ = []
myZip (a:b) (c:d) = (a,c):myZip b d