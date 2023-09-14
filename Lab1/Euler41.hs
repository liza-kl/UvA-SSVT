
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

-- By some rules of division, we can already filter some numbers out:
-- n =1, so the number would be "1" – 1 ist not prime 
-- n = 2, the possible combinations would be "12" or "21" – both can be divided by 3 – no prime
-- n = 3, the sum of the digits 1 + 2 + 3 is 6, divisible by 3, so every combination is (rules of division)
-- n = 5, the sum of the digits 1..5 is 15, divisible by 3, again rules of division every of those numbers is not prime
-- n = 6, the sum of the digits 1..6 is 21, divisible by 3, any 6 digit pandigital is divisible by 3
-- n = 8, the sum of the digits 1..8 is 36, divisible by 3, any 8 digit pandigital is divisible by 3
-- n = 9, the sum of the digits 1..9 is 45, divisible by 3, any 9 digit pandigital is divisible by 3

-- So we only need to consider 4 or 7 digit numbers for the problem
-- We are creating permutations for 1..4 and 1...7 and
-- find the number which isPrime and the maximum one

{-- 
Example for [1,2,3]
0 * 10 + 1 = 1
1 * 10 + 2 = 12
12 * 10 + 3 = 123 
--}
concatInts :: [Integer] -> Integer
concatInts = foldl (\acc x -> acc * 10 + x) 0

problem_41 = concatInts (maximum [ x | x <- permutations [1..4] ++ permutations [1..7], isPrime (concatInts x) ])
