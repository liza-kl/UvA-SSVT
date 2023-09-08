import Data.List
import System.IO

{-
  time spent: ~ 20 minutes
-}

-- we created a function for each condition

-- check that a number is prime
isPrime :: Integer -> Bool
isPrime n = all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) [2..]
   
-- check that the three numbers are permutations of each other
isPermutation :: Integer -> Integer -> Bool
isPermutation x y =
    sort (show x) == sort (show y)

primePermutations :: [(Integer, Integer, Integer)]
primePermutations = do
  let range = [1000..9999]
  -- work only with the primes of this range
  let primeNumbers = filter isPrime range
 
  -- check that all conditions are met 
  [(x, y, z) | x <- primeNumbers,
               let y = x + 3330,
               let z = y + 3330,
               y `elem` primeNumbers,
               z `elem` primeNumbers,
               isPermutation x y,
               isPermutation y z]

-- concatenate the 4-digit numbers to find the 12-digit ones
concatenateTuples :: [(Integer, Integer, Integer)] -> [Integer]
concatenateTuples tuples = map (\(x, y, z) ->  read (show x ++ show y ++ show z)) tuples

main :: IO ()
main = do
    let listOfPrimePermutations = concatenateTuples primePermutations
    print listOfPrimePermutations