import Data.List
import System.IO
import Test.QuickCheck

{- 
  time spent: ~ 2 hours mostly due to lack of syntax knowledge
  the input space for all 4 property tests is the actual list result of the program, 
  since that is what we care about. 
  besides, with a random list the tests would probably fail, since these properties are usually not true for any list.
  all 4 tests handle the edge case of an empty list input, returning True.
  there are no other edge cases, since the properties are pretty straight-forward.
  other properties of the final list are:
    1. there are no one-digit primes in it, since that would be useless information
    2. if we reversed all the elems in the list, we would still get the same list, in a different order
    3. the length of the list is less than 10K
    4. all numbers are positive, as primes
    etc
-}

reversibleStream :: [Integer] 
reversibleStream = do
    let y = [1..10000]

    -- find all primes
    let primeNumbers = filter isPrime y

    -- exclude one-digit numbers
    let excludeOneDigitPrimes = filter isBiggerThan9 primeNumbers

    -- reverse them
    let reversals = map reversal excludeOneDigitPrimes

    -- get primes
    let primeReversals = filter isPrime reversals

    primeReversals


reversal :: Integer -> Integer
reversal = read . reverse . show

isPrime :: Integer -> Bool
isPrime n = all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) [2..]

isBiggerThan9 :: Integer -> Bool
isBiggerThan9 = (>9)

-- 3rd property
listIsPrime :: [Integer] -> Bool
listIsPrime [] = True
-- check that all elems are prime
listIsPrime x = all isPrime x

-- 5th property
reversalSymmetry :: [Integer] -> Bool
reversalSymmetry [] = True
-- check for every elem that its reversal also in the list
reversalSymmetry xs = all (\x -> reversal x `elem` xs) xs

-- 6th property
isUnique :: [Integer] -> Bool
isUnique [] = True
-- check if first elem and the rest are unique, recursively
isUnique (x:xs) = x `notElem` xs && isUnique xs

-- 7th property
isLessThan10000 :: [Integer] -> Bool
isLessThan10000 [] = True
-- check for each elem that it has a value smaller than 10K
isLessThan10000 xs = all (\x -> x < 10000) xs


main :: IO ()
main = do
    let primeReversals = reversibleStream
    putStrLn (show primeReversals)

    quickCheckWith stdArgs $ forAll (return primeReversals) isUnique
    quickCheckWith stdArgs $ forAll (return primeReversals) reversalSymmetry  
    quickCheckWith stdArgs $ forAll (return primeReversals) isLessThan10000
    quickCheckWith stdArgs $ forAll (return primeReversals) listIsPrime
    