module Exercise8 where

import Data.List 

-- Exercise 8
-- Time needed 2,5h due to trying to optimize list generation and other issues related to that.

-- Generate an infinite list of prime numbers.
listOfPrimes:: [Integer]
listOfPrimes = filter isPrime [2..]

{-
  Generates an infinitely sized list of lists with prime numbers which get consecutively larger for the next element.
  Example: [[2], [2, 3], [2,3,5], (...)]
-}
getConsecutivelyLargerPrimesList:: [a] -> [[a]] -> [[a]]
getConsecutivelyLargerPrimesList [] _ = []
getConsecutivelyLargerPrimesList xs acc = sublist : getConsecutivelyLargerPrimesList remaining (acc ++ [sublist])
  where
    sublist = take (length acc + 1) xs
    remaining = xs ++ drop (length acc + 1) xs

getConsecutivelyLargerPrimes:: [[Integer]]
getConsecutivelyLargerPrimes = getConsecutivelyLargerPrimesList listOfPrimes []

-- Multiplies every element of an integer list together and add one to the final result.
multiplyAdd1 :: [Integer] -> Integer
multiplyAdd1 list1 = product list1 + 1

isPrime :: Integer -> Bool
-- Test Report
-- It gets stuck on certain prime numbers, but if you would insert a number which is bigger than the one
-- it gets stuck, the tests continue (?) 
-- isPrime 7858321551080267055879091 = False
isPrime n = all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) [2..]

-- Generates an element where we have the prime number calculation result with the corresponding prime number list.
-- Exists to to easier counter example visualization.
mapListStructure:: [Integer] -> ([Integer], Integer)
mapListStructure primes = (primes, multiplyAdd1 primes)

-- Generates the counterexamples to this conjecture.
counterexamples :: [([Integer], Integer)]
counterexamples = filter (\(subs, p) -> not(isPrime p)) ( map mapListStructure getConsecutivelyLargerPrimes)