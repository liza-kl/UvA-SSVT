


import Data.List

-- Time Spent 90 Minutes due to lack of Haskell Syntax knowledge

-- Do you have to test that your answer is correct? How could this be checked?
-- Probably with some kind of induction, 
-- we could prove that our sequence of numbers is monotonous increasing, therefore the sum in the afterwards
-- iteration can't be smaller one than the current iteration.
-- Due to the fact that we are checking for the prime property
--  in each iteration, we stop at the first case where it actually evaluates to True

-- Thinking behind the "logic"
-- (1) We need a function that checks, if a number is a prime
-- (2) We need a function that creates consecutive primes from a start prime
-- (3) We need a function that creates 101 consecutive prime numbers
-- (4) We need a function that sums the list of primenumbers that we get
-- (5) We need to check with (2) if the sum is a prime number
-- (5.1) If yes, we are returning the current generated list
-- (5.2) If not, we are going to recurse by removing the
-- first element from the current list and taking 
-- second element from the list as our new start prime number

-- The function for (1)
isPrime :: Integer -> Bool 
isPrime v = let maxDiv = floor(sqrt (fromIntegral v))
            in all (\x -> (v `rem` x) /= 0) [2..maxDiv]

-- The function for (2) with the sieve of eratosthenes
primes x = sieve [x..]
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

-- The function for (3) which takes a start prime
-- and takes 101 numbers from the creates "infinite" list 

getPrimes :: Integer -> [Integer]
getPrimes startPrime = take 101 $ primes startPrime

-- The function for 4 

getPrimeSum :: Integer -> Integer
getPrimeSum startPrime = sum (getPrimes startPrime)

getSmallestPrime :: Integer -> [Integer]
getSmallestPrime x 
    | isPrime (getPrimeSum x) = getPrimes x
    | otherwise = getSmallestPrime (head (tail (getPrimes x)))
    
consecutive101Prime :: Integer

consecutive101Prime = sum ( getSmallestPrime 2 ) -- output 24151
