module TypeProblems where


data Answer a = Impossible | Example a


-- Give Example definitions that have the following types if it is possible. If not return "Impossible".  It does not matter what the definitions actually do as long AS they are correctly typed, they do not "loop forever", and they do not use undefined.

q1 :: Answer [Bool]
q1 = Example [True]

q2 :: Answer ([(Int,Char)],Bool)
q2 = Example ([(1, '1')], True)

q3 :: Answer (Int -> Bool -> Char -> Int -> Bool)
q3 = Example (\x y z k->y)

q4 :: Answer (a -> ([a],[a],[a]))
q4 = Example (\x->([x],[x],[x]))


q5 :: Answer (a -> b)
q5 = Impossible

q6 :: Answer (a -> a)
q6 = Example (\x -> x)

-- but different then above
q7 :: Answer (a -> a)
q7 = Impossible

q8 :: Answer (a -> a -> a)
q8 = Example (\x y-> x)

-- but different then above
q9 :: Answer (a -> a -> a)
q9 = Example (\y x-> x)


q10 :: Answer (a -> b -> a)
q10 = Example (\x y-> x)

q11 :: Answer (a -> b -> b)
q11 = Example (\x y-> y)

q12 :: Answer ((a -> b) -> b)
q12 = Impossible

q13 :: Answer ((a -> b) -> (Maybe b))
q13 = Example (\x->Nothing)

q14 :: Answer (a -> b -> (b,a))
q14 = Example (\x y->(y,x))

q15 :: Answer ((a -> b) -> (b -> c) -> (a -> c))
q15 = Example (\x y-> y . x)

q16 :: Answer ((a -> b) -> (b -> c) -> (c -> a))
q16 = Impossible

q17 :: Answer ((a -> b) -> ([b] -> c) -> c)
q17 = Impossible
