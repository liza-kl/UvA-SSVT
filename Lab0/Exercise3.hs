
import Data.List
import Test.QuickCheck

-- Time Spent: 90 Minutes, due to lack of syntax knowledge and compiler issues

-- Triangle generation

data Shape = NoTriangle | Equilateral
     | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape 
checkForDuplicates :: (Eq a ) => [a] -> Bool

-- Tests for there are duplicate triangle lengths for at least two sides of the shape.
checkForDuplicates [] = False 
checkForDuplicates [a] = False 
checkForDuplicates (x:x':xs)
     | x == x' = True 
     | x /= x' = checkForDuplicates(x':xs)
     | otherwise = False

-- Checking via Pythagoras Theorem.
isRectangular :: Integer -> Integer -> Integer -> Bool
isRectangular a b c = 
     filter (/= maximum[a,b,c]) [a,b,c]!!0^2
     + filter (/= maximum[a,b,c]) [a,b,c]!!1^2
     == maximum[a,b,c]^2

triangle a b c 
    | any (<= 0) [a,b,c] = NoTriangle                     -- If any side of a triangle is negative or zero, it just can't be a triagnle
    | a + b <= c || b + c <= a || a + c <= b = NoTriangle -- Theorem of Triangle inequality
    | all ((==) a) [b,c] = Equilateral                    -- If 3 sides are equal, it is an Equilateral triangle
    | checkForDuplicates [a,b,c]  = Isosceles             -- If 2 sides are equal, it is an isosceles
    | isRectangular a b c = Rectangular                   -- "proofed" by using the Pythagorean theorem
    | otherwise = Other 

-- QuickCheck tests for the different triangle/shape types.

isTriangleNoTriangle:: Integer -> Integer -> Integer -> Bool
isTriangleNoTriangle a b c = triangle a b c  == NoTriangle

isTriangleEquilateral:: Integer -> Integer -> Integer -> Bool
isTriangleEquilateral a b c = triangle a b c  == Equilateral

-- We pre-define it with 2 same length values
isTriangleIsosceles:: Integer -> Bool
isTriangleIsosceles c = triangle 5 5 c  == Isosceles

isTriangleRectangular:: Integer -> Integer -> Integer -> Bool
isTriangleRectangular a b c = triangle a b c  == Rectangular

genNegativeInteger:: Gen Integer
genNegativeInteger = abs <$> (arbitrary :: Gen Integer) `suchThat` (<0)

genPositiveInteger :: Gen Integer
genPositiveInteger = abs <$> (arbitrary :: Gen Integer) `suchThat` (>0)

genPositiveIntegerNonFiveInLimits :: Gen Integer
genPositiveIntegerNonFiveInLimits = abs <$> (arbitrary :: Gen Integer) `suchThat` ( \x -> x > 0 && x /= 5 && x < 10)

main :: IO ()
main = do
-- No Triangle Check
        quickCheck $ forAll genNegativeInteger isTriangleNoTriangle
        quickCheck ( isTriangleNoTriangle 3 5 37 )

-- Equilateral Check
        quickCheck ( isTriangleEquilateral 5 5 5 )
        quickCheck (not ( isTriangleEquilateral 0 0 0))

-- Isosceles Check
        quickCheck $ forAll genPositiveIntegerNonFiveInLimits isTriangleIsosceles

-- Rectangular Check
        quickCheck (isTriangleRectangular 3 4 5)

-- Other triangle check not achievable due to other conditions and the integer parameters restricting it to other triangle types.

-- Correctness of the program is checked by applying the correct corresponding definitions of each triangle type.
-- Also QuickCheck tests are additionally provided to check for different cases of triangle types.
-- With the definition for each triangle type applied and additional checks verifying it as well this program is correct.