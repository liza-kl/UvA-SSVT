-- Exam 01022023 Problem 3.1 

-- Let S(n) denote the set of all symmetric relations on a set of cardinality n.
-- Consider the following implementation of a function numS:: Int -> Int that computes the cardinal of
-- S(n) without generating all its elements.

numS :: Int -> Int 
numS 0 = 1 -- Probably the empty set that counts 
numS n = 2^n * numS (n - 1) 

-- 1.) Is the numS implementation correct? Provide an induction proof for your answer.

numR 0 = 1
numR n = 2^(2*n-2) * numR (n-1)

-- numR' 0 = 1
numR' n = 2^(n^2-n) 
