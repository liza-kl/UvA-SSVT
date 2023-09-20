-- Time spent: 30 minutes, lack of knowledge about Haskell Syntax 

module Exercise1 where
import Test.QuickCheck
    ( suchThat, forAll, quickCheck, Arbitrary(arbitrary), Gen )


factorial :: Integer -> Integer

-- Factorial of 0 = 1
prop_factorialOfZeroIsOne :: Bool
prop_factorialOfZeroIsOne = factorial 0 == 1

-- For any positive integer (n! / n) = (n - 1)!
prop_factorialOfAnyPosInteger :: Integer -> Bool
prop_factorialOfAnyPosInteger num = (factorial num `div` num) == factorial (num - 1)

-- Factorial is always > 0
prop_factorialIsAlwaysGreater0 :: Integer -> Bool
prop_factorialIsAlwaysGreater0 num = factorial num > 0

-- Divison Rule in Factorial : x! / ( x - 1 )! = x | x >= 1 
prop_factorialDivisionRule :: Integer -> Bool
prop_factorialDivisionRule num = (factorial num `div` factorial (num - 1)) == num


genNonNegativeIntsGe1 :: Gen Integer
genNonNegativeIntsGe1 = abs `fmap` (arbitrary :: Gen Integer) `suchThat` (>= 1)

genPositiveInts :: Gen Integer
genPositiveInts = abs `fmap` (arbitrary :: Gen Integer) `suchThat` (> 0)

genNonNegativeInts :: Gen Integer
genNonNegativeInts = abs `fmap` (arbitrary :: Gen Integer) `suchThat` (>= 0)

factorial num
    | num < 0 = error "Negative numbers are not possible"
    | num == 1 = 1
    | num == 0 = 1
    | otherwise = num * factorial (num - 1)

main :: IO()
main = do
   quickCheck $ forAll genPositiveInts prop_factorialOfAnyPosInteger
   quickCheck $ forAll genNonNegativeInts prop_factorialIsAlwaysGreater0
   quickCheck prop_factorialOfZeroIsOne
   quickCheck $ forAll genNonNegativeIntsGe1 prop_factorialDivisionRule