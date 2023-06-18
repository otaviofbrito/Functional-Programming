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