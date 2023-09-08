


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

smallestPrimeNum :: Integer -> IO Integer
smallestPrimeNum x = do
    let y = [x..]

    -- find all primes
    let primeNumbers = filter isPrime y

    -- get first 101 primes
    let first101Primes = take 101 primeNumbers

    -- mapM_ print first101Primes

    -- get sum of 101 primes
    let sumOf101Primes = sum first101Primes

    -- print sumOf101Primes

    -- check if the sum is a prime number
    let isSumPrime = isPrime sumOf101Primes :: Bool

    -- if so, return
    if isSumPrime
        then return sumOf101Primes
    -- otherwise search again starting from the next prime, recursively
        else smallestPrimeNum(x + 1)

isPrime :: Integer -> Bool
isPrime n = all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) [2..]

main :: IO ()
main = do
    sumOf101Primes <- smallestPrimeNum 2
    putStrLn (show sumOf101Primes)
