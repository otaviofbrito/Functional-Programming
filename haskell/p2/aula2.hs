import Data.Char


geraComb::Int->[Int]->[(Int,Int)]
geraComb _ [] = []
geraComb x (a:b) = (x,a):geraComb x b

pdCartesiano::[Int]->[Int]->[(Int,Int)]
pdCartesiano [] _ = []
pdCartesiano _ [] = []
pdCartesiano (a:b) x = geraComb a x ++ pdCartesiano b x 

produtoCartesiano::[Int]->[(Int,Int)]
produtoCartesiano l = [(a,b) | a<-l , b<-l]


produtoCartesiano2::[Int]->[Int]->[(Int,Int)]
produtoCartesiano2 l1 l2= [(x,y) | x <- l1, y <- l2]


relacaoMaior::[Int]->[Int]->[(Int,Int)]
relacaoMaior l1 l2 = [(x,y) | x <- l1, y <- l2 , x>y]

relacaoMaior2::[(Int,Int)]->[(Int,Int)]
relacaoMaior2 [] = []
relacaoMaior2 ((a,b):c)
  |a > b = (a,b):relacaoMaior2 c
  |otherwise = relacaoMaior2 c


identidade::[Int]->[Int]->[(Int,Int)]
identidade l1 l2 = [(x,y) | x <- l1, y<- l2, x==y]

qtdX::Int->[Int]->Int
qtdX _ [] = 0
qtdX x (a:b)
  |x == a = 1 + qtdX x b
  |otherwise = qtdX x b


func1::[(Int,[Int])]->[(Int,Int)]
func1 l = [(x,qtdX x y) | (x,y) <- l]


zip1::[Int]->[Int]->[(Int,Int)]
zip1 [] _ = []
zip1 _ [] = []
zip1 (a:b) (c:d) = (a,c):zip1 b d

par::Int->Bool
par x = x `mod` 2 == 0

insereOrdenado:: Int->[Int]->[Int]
insereOrdenado x [] = [x]
insereOrdenado x (a:b)
  |x <= a = x:a:b
  |otherwise = a:insereOrdenado x b


ordena::[Int]->[Int]
ordena [x] = [x]
ordena (a:b) = insereOrdenado a (ordena b)

{-
func2::[Int]->[Int]->([Int],[Int])
func2 l1 l2 = (ordena [x| x <- l1 , par x], ordena [x| x <- l2, not(par x)])
-}


func3::Char->String->(Char,[Int])
func3 c s = (c,[y | (x,y) <- zip s [1..], c==x ])


checkPos::(Int,Char,String)->Bool
checkPos (_,_,[]) = False
checkPos (x,c,(a:b))
  | x == 1 && c==a = True
  | otherwise = checkPos(x-1,c,b)


func4::[(Int,Char,String)]->([(Int,Char,String)], [(Int,Char,String)])
func4 l = ([x| x <- l, checkPos x], [x | x <- l, not(checkPos x)])
 

