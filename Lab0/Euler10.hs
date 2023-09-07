import Data.List
import System.IO


--  time spent: ~ 5 minutes

isPrime :: Integer -> Bool
isPrime n = all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) [2..]

euler10 :: Integer
euler10 = do
    let y = [1..2000000]
    -- finds all the primes
    let primeNumbers = filter isPrime y
    -- sums them up
    let sumOfNums = sum primeNumbers
    sumOfNums


main :: IO ()
main = do
    let sumOfNums = euler10
    putStrLn (show sumOfNums)