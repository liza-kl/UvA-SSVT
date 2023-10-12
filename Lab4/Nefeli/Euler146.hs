module Euler146 where
import Test.QuickCheck
import Data.List
import System.Random
import SetOrd

isPrime :: Integer -> Bool
isPrime n = all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) [2..]

hasPrimePattern :: Integer -> Bool
hasPrimePattern n = isPrime (n*n + 1) && isPrime (n*n + 3) && isPrime (n*n + 7) && isPrime (n*n + 9) && isPrime (n*n + 13) && isPrime (n*n + 27)

main :: IO ()
main = do
    let numbers = [1..150000000]
    let filteredNumbers = filter hasPrimePattern numbers
    let filteredSum = sum filteredNumbers
    putStrLn (show filteredSum)