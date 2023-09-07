
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
    | any (<= 0) [a,b,c] = NoTriangle
    | a + b <= c || b + c <= a || a + c <= b = NoTriangle
    | all ((==) a) [b,c] = Equilateral
    | checkForDuplicates [a,b,c]  = Isosceles
    | isRectangular a b c = Rectangular
    | otherwise = Other 

