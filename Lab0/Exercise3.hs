
import Data.List

-- Time Spent: 90 Minutes, due to lack of syntax knowledge and compiler issues

data Shape = NoTriangle | Equilateral
     | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape 
checkForDuplicates :: (Eq a ) => [a] -> Bool

checkForDuplicates [] = False 
checkForDuplicates [a] = False 
checkForDuplicates (x:x':xs)
     | x == x' = True 
     | x /= x' = checkForDuplicates(x':xs)
     | otherwise = False


isRectangular :: Integer -> Integer -> Integer -> Bool
isRectangular a b c = 
     filter (/= maximum[a,b,c]) [a,b,c]!!0^2
     + filter (/= maximum[a,b,c]) [a,b,c]!!1^2
     == maximum[a,b,c]^2

triangle a b c 
    | any (<= 0) [a,b,c] = NoTriangle -- If any side of a triangle is negative or zero, it just can't be a triagnle
    | a + b <= c || b + c <= a || a + c <= b = NoTriangle -- Theorem of Triangle inequality
    | all ((==) a) [b,c] = Equilateral -- If 3 sides are equal, it is an Equilateral triangle
    | checkForDuplicates [a,b,c]  = Isosceles -- If 2 sides are equal, it is an isosceles
    | isRectangular a b c = Rectangular -- "proofed" by using the Pythagorean theorem
    | otherwise = Other 

