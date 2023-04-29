soma::Int->Int->Int
soma a b = a+b;

mult::Int->Int->Int
mult a 1 = a
mult a b = soma a (mult a (b-1))