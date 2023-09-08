
import Test.QuickCheck

-- Time Spent: 20 Minutes (looking up QuickCheck Tutorials and Haskell Syntax)
-- How would you prove that these statements are true?
-- Induction

firstFunction :: Int -> Int
firstFunction n = sum [x^2 | x <- [1..n]]

secondFunction :: Integer -> Integer
secondFunction n = sum [x^3 | x <- [1..n]]

firstFunctionProp n =
    (n >= 0) ==>
    firstFunction n === n * (n + 1) * (2*n + 1) `div` 6
secondFunctionProp n =
    (n >= 0) ==>
    secondFunction n === (n * ((n + 1) `div` 2))^2