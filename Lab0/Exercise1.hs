module Main where

import Test.QuickCheck

{- 
    Time spent: Two hours; getting used to the syntax
    Also, we used integer division `div` to get the integer part. 
    Otherwise, the difference in float number precision would falsify the equality.
    Mathematically, we could prove equality by induction. See below.
-}

{-
    Proof by induction for each of the given statements.
    1) 
        I) n = 1 -> 1 (1 + 1)(2*1 + 1) / 6 = 1 -> Sum of 1 square is also one -> True
        II) We assume that the statement is true for all natural numbers.
        III) Proof it is still correct for n+1
            -> n (n + 1) (2n + 1) / 6 + (n + 1)^2
            -> (n (n + 1) (2n + 1) + 6*(n + 1)^2) / 6
            -> ((n+1) (2n^2 + n) + 6*(n + 1)^2) / 6
            -> (2n^3 + 9n^2 + 13n + 6) / 6
            -> (n + 1) (2n^2 + 7n + 6) / 6
            -> (n + 1) ((n + 2) (2n + 3)) / 6
            -> (n + 1) ((n + 1) + 1) (2*(n + 1) + 1) / 6
            -> Statement is true. 
    2) 
        I) n = 1 -> (1(1 + 1)/2) = 2/2 = 1 -> Sum of 1 cubed is also one -> True
        II) We assume that the statement is true for all natural numbers.
        III) Proof it is still correct for n+1
            -> (n(n + 1) / 2)^2 + (n + 1)^3
            -> ((n^4 + 2n^3 + n^2) / 4) + (4*(n + 1)^3 / 4)
            -> ((n^4 + 2n^3 + n^2) / 4) + (4n^3 + 12n^2 + 12n + 4) / 4
            -> (n^4 + 6n^3 + 13n^2 + 12n + 4) / 4
            -> (n + 1)^2 (n + 2)^2 / 4
            -> ((n+1) ((n+1) + 1) / 2)^2
            -> Statement is true.
-}

-- First equation
sumOfNSquares :: Integer -> Integer
sumOfNSquares' :: Integer -> Integer

sumOfNSquares n = sum [ k^2 | k <- [1..n]]
sumOfNSquares' n = n * (n+1) * (2 * n + 1) `div` 6 


testSumOfNSquares :: Integer -> Bool

testSumOfNSquares n = sumOfNSquares n == sumOfNSquares' n

-- Randomization of the input for the QuickCheck test
genPositiveIntegers:: Gen Integer
genPositiveIntegers = abs <$> (arbitrary :: Gen Integer) `suchThat` (>0)


-- Second equation
sumOfNCubes :: Integer -> Integer
sumOfNCubes' :: Integer -> Integer

sumOfNCubes n = sum [ k^3 | k <- [1..n]]
sumOfNCubes' n = (n * (n+1) `div` 2)^2

testSumOfNCubes :: Integer -> Bool

testSumOfNCubes n = sumOfNCubes n == sumOfNCubes' n

-- Output with QuickCheck tests
main :: IO Result
main = do
        quickCheckResult $ forAll genPositiveIntegers testSumOfNSquares
        quickCheckResult $ forAll genPositiveIntegers testSumOfNCubes
    
