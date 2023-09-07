
import Data.List

data Shape = NoTriangle | Equilateral
     | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape 
checkForDuplicates :: (Eq a ) => [a] -> Bool
-- removeElementFromList:: a -> [a] -> [a]


checkForDuplicates [] = False 
checkForDuplicates [a] = False 
checkForDuplicates (x:x':xs)
     | x == x' = True 
     | x /= x' = checkForDuplicates(x':xs)
     | otherwise = False

-- removeElementFromList _ [] = []
-- removeElementFromList a x:xs
--      | a == x = [xs]
--      | otherwise [a:xs]

-- isRectangular :: Integer -> Integer -> Integer -> Bool
-- isRectangular a b c = 
--      let hypothenuse = maximum[a,b,c]
--      let residuals = 

triangle a b c 
    | any (<= 0) [a,b,c] = NoTriangle
    | a + b <= c || b + c <= a || a + c <= b = NoTriangle
    | all ((==) a) [b,c] = Equilateral
    | checkForDuplicates [a,b,c]  = Isosceles
  --  | (\x y z <- (maximum[x,y,z] ) a b c) = Rectangular
    | otherwise = Other 

