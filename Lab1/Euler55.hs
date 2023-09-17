module Euler55 where

import Data.List

-- To test that this function works as expected I would run property tests
-- Some properties could be:
-- I could check isPalindrome and reversal with QuickCheck
-- The result should be a positive number and < 10000
-- Other than that, it's hard to test that the isLychrel function works well, 
-- in an automated way, since these numbers are not known

lychrelNumbers :: IO Int
lychrelNumbers = do
    lychrelNums <- isLychrel 10 1 [] 10 
    -- putStrLn (show lychrelNums)
    return (length lychrelNums)

isLychrel :: Integer -> Integer -> [Integer] -> Integer -> IO [Integer]
-- x is the number i initially started working on, now im recursively looking into its sum etc 
-- i the iteration, 
-- lychrelNumbers is the list with the lychrel numbers,
-- y is the number im working on
isLychrel x i lychrelNumbers y
    -- reached 50 iterations, go to next numbers
    | i == 50 = do
        isLychrel (x + 1) 1 (x : lychrelNumbers) (x + 1)
    -- reached 10000 numbers, stop search
    | x == 10000 = do
        return lychrelNumbers
    -- the sum with the reverse is palindrome, then go to next number
    | condition = do
        isLychrel (x + 1) 1 lychrelNumbers (x + 1)
    -- check reverse of the sum, and do everything again - for the same number
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
