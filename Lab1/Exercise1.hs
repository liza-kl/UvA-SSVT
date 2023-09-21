-- Time spent: 30 minutes, lack of knowledge about Haskell Syntax 
-- Properties source: https://functions.wolfram.com/introductions/PDF/Factorial.pdf

module Exercise1 where
import Test.QuickCheck
    ( suchThat, forAll, quickCheck, Arbitrary(arbitrary), Gen, Property, property )

factorial :: Integer -> Integer
factorial num
    | num < 0 = error "Negative numbers are not possible"
    | num == 1 = 1
    | num == 0 = 1
    | otherwise = num * factorial (num - 1)

-- We check for the property that !0 = 1. Factorial of 0 = 1
prop_factorialOfZeroIsOne :: Property
prop_factorialOfZeroIsOne = property (factorial 0 == 1)

-- For any positive integer (n! / n) = (n - 1)! | n < 0
prop_factorialOfAnyPosInteger :: Integer -> Property
prop_factorialOfAnyPosInteger num = property ((factorial num `div` num) == factorial (num - 1))

-- Factorial is always > 0
prop_factorialIsAlwaysGreater0 :: Integer -> Property
prop_factorialIsAlwaysGreater0 num = property ( factorial num > 0)

-- Divison Rule in Factorial : x! / ( x - 1 )! = x | x >= 1 
prop_factorialDivisionRule :: Integer -> Property
prop_factorialDivisionRule num = property ((factorial num `div` factorial (num - 1)) == num)

-- Generating numbers greater equal than for the "prop_factorialDivisionRule",
-- because division through 0 is not possible
genNonNegativeIntsGe1 :: Gen Integer
genNonNegativeIntsGe1 = abs `fmap` (arbitrary :: Gen Integer) `suchThat` (>= 1)

-- Generating strictly positive integers for checking the prop "prop_factorialOfAnyPosInteger"
genPositiveInts :: Gen Integer
genPositiveInts = abs `fmap` (arbitrary :: Gen Integer) `suchThat` (> 0)

-- generating non negative integers to show that the factorial is always greater than 0
genNonNegativeInts :: Gen Integer
genNonNegativeInts = abs `fmap` (arbitrary :: Gen Integer) `suchThat` (>= 0)

{-- Test Report: 
- All generated tests passed. Maybe there would have been a better approach to generate
the numbers to avoid duplication code. 
-- We can assume, based on the tested properties, that our function is working correctly.
--}

main :: IO()
main = do
   quickCheck $ forAll genPositiveInts prop_factorialOfAnyPosInteger
   quickCheck $ forAll genNonNegativeInts prop_factorialIsAlwaysGreater0
   quickCheck prop_factorialOfZeroIsOne
   quickCheck $ forAll genNonNegativeIntsGe1 prop_factorialDivisionRule