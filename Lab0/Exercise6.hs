


import Data.List

-- Time Spent 90 Minutes due to lack of Haskell Syntax knowledge

-- 1.) a function which checks if we have a prime 
-- 2.) function that generates 101 consecutive primes -- list
-- 3.) check if sum of a list is a prime number, if not cut the head, 
    -- generate the next consectuive prime number to the end

isPrime :: Int -> Bool
isPrime v = let maxDiv = floor(sqrt (fromIntegral v))
            in all (\x -> (v `rem` x) /= 0) [2..maxDiv]

primes x = sieve [x..]
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

getPrimes :: Int -> [Int]
getPrimes startPrime = take 101 $ primes startPrime

getPrimeSum :: Int -> Int
getPrimeSum startPrime = sum (getPrimes startPrime)

getSmallestPrime x 
    | isPrime (getPrimeSum x) = getPrimes x
    | otherwise = getSmallestPrime (head (tail (getPrimes x)))
    

