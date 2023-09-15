module Euler55 where

import Data.List

-- TODO : explain how it could be tested

lychrelNumbers :: IO Int
lychrelNumbers = do
    lychrelNums <- isLychrel 10 1 [] 10
    -- putStrLn (show lychrelNums)
    return (length lychrelNums)

isLychrel :: Integer -> Integer -> [Integer] -> Integer -> IO [Integer]
isLychrel x i lychrelNumbers y
    | i == 50 = do
        -- putStrLn ("Reached 50 iterations for x = " ++ show x)
        isLychrel (x + 1) 1 (x : lychrelNumbers) (x + 1)
    | x == 10000 = do
        -- putStrLn "Stopping search"
        return lychrelNumbers
    | condition = do
        -- putStrLn (show condition ++ " " ++ show addReverse ++ " is a palindrome for x = " ++ show x)
        isLychrel (x + 1) 1 lychrelNumbers (x + 1)
    | otherwise = do
        -- putStrLn (show condition ++ " " ++ show addReverse ++ " is not a palindrome for x = " ++ show x ++ " checking " ++ show addReverse)
        isLychrel x (i + 1) lychrelNumbers addReverse
  where
    reverseNum = reversal y
    addReverse = reverseNum + y
    condition = isPalindrome addReverse

isPalindrome :: Integer -> Bool
isPalindrome x = (show x) == reverse (show x)

reversal :: Integer -> Integer
reversal = read . reverse . show

main :: IO ()
main = do
    numOfLychrelNumbers <- lychrelNumbers
    putStrLn (show numOfLychrelNumbers)
