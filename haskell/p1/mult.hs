--multiply 2 numbers using only sum operator

mult::Int->Int->Int
mult a 1 = a
mult a b = a + mult a (b-1)

