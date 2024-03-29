--1
sqList::[Int]->Int
sqList l = sqSum [a^2 | a<-l]

sqSum::[Int]->Int
sqSum [] = 0
sqSum (a:b) = a + sqSum b

--2
myReplicate::Int->a->[a]
myReplicate x t = [t | _ <- [1..x]]

myReplicate2::Int->a->[a]
myReplicate2 0 _ = []
myReplicate2 x y = y:myReplicate2 (x-1) y

listLen::[Int]->Int
listLen l = sqSum [1 | _ <- l]

listLen2::[Int]->Int
listLen2 [] = 0
listLen2 (a:b) = 1 + listLen2 b


--3
pyths::Int->[(Int,Int,Int)]
pyths n = [(x,y,z)|x<-[1..n], y <- [1..n] , z<-[1..n], (x^2+y^2)==z^2]


--4

factors::Int->[Int]
factors x = [a | a <- [1..x-1], x `mod` a == 0 ] 


perfects::Int->[Int]
perfects x = [a | a <- [1..x], sqSum(factors a) == a]

--5
gen2::[Int]->[Int]->[[(Int,Int)]]
gen2 l1 l2 = [[(x,y) | y <- l2] | x <- l1]

gen1::[Int]->[Int]->[(Int,Int)]
gen1 l1 l2 = [(x,y) | x <- l1,  y <- l2]

--6
find1::(Eq a)=>a->[(a,Int)]->[Int]
find1 c l = [y | (x,y) <- l , x == c]

--7

scalarProduct::[Int]->[Int]->Int
scalarProduct l1 l2 = sqSum[x*y| (x,y) <- zip l1 l2]


--8
infixr 8 &!

(&!)::Int->Int->Int
_ &! 0 = 1
x &! y = x * (&!) x (y-1)

--9
lcFunc::(Int->Int)->(Int->Bool)->[Int]->[Int]
lcFunc op p l = [op x | x <-l , p x ]

fmFunc::(Int->Int)->(Int->Bool)->[Int]->[Int]
fmFunc op p l = map op (filter p l)

