
{-- 
We shall say that an n
-digit number is pandigital if it makes use of all the digits 1 to n
exactly once. For example, 2143
 is a 4
-digit pandigital and is also prime.

What is the largest n
-digit pandigital prime that exists?
--}

import Data.Char (intToDigit)
import Data.List

isPrime :: Integer -> Bool -- Taken from previous Euler problems 
isPrime n = all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) [2..]

-- We only need to consider numbers with at _least_ 3 digits, because no 1-digit nummber is prime 
-- it could only be "1" and 1 is not prime
-- No two-digit number is prime because you can only have 12 (can be divided 6,2) or 21 (can be divided by 3, 7)
-- so both are not pandigital, becase not prime 
-- 3 digit numbers: 
problem_41 = maximum [ n' | d <- [4,7], n <- permute ['1'..intToDigit d],
                            let n' = read n, isPrime n']
    where
        permute "" = [""]
        permute str = [x:xs| x <- str, xs <- permute (delete x str)]