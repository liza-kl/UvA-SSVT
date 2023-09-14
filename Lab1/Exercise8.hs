import Data.List 

--- Exercise 8

listOfPrimes:: [Integer]
listOfPrimes = filter isPrime [2..]

getPrimes:: Int -> [Integer]
getPrimes amount = take amount listOfPrimes


getPrimesList:: [[Integer]]
getPrimesList = getPrimesList' 0

-- TODO IMPROVE THIS...
getPrimesList':: Int -> [[Integer]]
getPrimesList' idx = getPrimes idx : getPrimesList' (idx + 1)

comparePrimesList:: [Integer] -> Bool
comparePrimesList testedPrimes = testedPrimes == getPrimes (length testedPrimes)

multiplyAdd1 :: [Integer] -> Integer
multiplyAdd1 list1 = product list1 + 1

isPrime :: Integer -> Bool
isPrime n = all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) [2..]

mapListStructure:: [Integer] -> ([Integer], Integer)
mapListStructure primes = (primes, multiplyAdd1 primes)

counterexamples :: [([Integer], Integer)]
counterexamples = filter (\(subs, p) -> not(isPrime p)) ( map mapListStructure getPrimesList)