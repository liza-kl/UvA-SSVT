module Exercise8 where

import Data.List 

-- Exercise 8
-- Time needed 2,5h due to trying to optimize list generation and other issues related to that.

listOfPrimes:: [Integer]
listOfPrimes = filter isPrime [2..]

shrinkPrimes:: [a] -> [[a]] -> [[a]]
shrinkPrimes [] _ = []
shrinkPrimes xs acc = sublist : shrinkPrimes remaining (acc ++ [sublist])
  where
    sublist = take (length acc + 1) xs
    remaining = xs ++ drop (length acc + 1) xs

getShrinkingPrimes:: [[Integer]]
getShrinkingPrimes = shrinkPrimes listOfPrimes []

multiplyAdd1 :: [Integer] -> Integer
multiplyAdd1 list1 = product list1 + 1

isPrime :: Integer -> Bool
-- Somehow it gets stuck on certain prime numbers...
-- isPrime 7858321551080267055879091 = False
isPrime n = all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) [2..]

mapListStructure:: [Integer] -> ([Integer], Integer)
mapListStructure primes = (primes, multiplyAdd1 primes)

counterexamples :: [([Integer], Integer)]
counterexamples = filter (\(subs, p) -> not(isPrime p)) ( map mapListStructure getShrinkingPrimes)