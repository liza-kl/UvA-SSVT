module Main where
-- import  Exercise1
import Test.QuickCheck

{- 
time spent: two hours; getting used to the syntax
For the purpose of QuickCheck, we assumed n is a natural number.
Also, we used integer division `div` to get the integer part. 
Otherwise,the difference in float number precision would falsify the equality.
Mathematically, we could prove equality by induction. 
-}

-- first equation
sumOfNSquares :: Integer -> Integer
sumOfNSquares' :: Integer -> Integer

sumOfNSquares n = sum [ k^2 | k <- [1..n]]
sumOfNSquares' n = n * (n+1) * (2 * n + 1) `div` 6 


testSumOfNSquares :: Integer -> Bool

testSumOfNSquares n = sumOfNSquares n == sumOfNSquares' n

-- randomization of the input
genPositiveIntegers:: Gen Integer
genPositiveIntegers = abs <$> (arbitrary :: Gen Integer) `suchThat` (>0)


--second equation
sumOfNCubes :: Integer -> Integer
sumOfNCubes' :: Integer -> Integer

sumOfNCubes n = sum [ k^3 | k <- [1..n]]
sumOfNCubes' n = (n * (n+1) `div` 2)^2

testSumOfNCubes :: Integer -> Bool

testSumOfNCubes n = sumOfNCubes n == sumOfNCubes' n

-- output
main :: IO Result
main = do
        quickCheckResult $ forAll genPositiveIntegers testSumOfNSquares
        quickCheckResult $ forAll genPositiveIntegers testSumOfNCubes
    
