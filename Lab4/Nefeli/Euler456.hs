module Euler456 where
import Test.QuickCheck
import Data.List 
import System.Random
import SetOrd
import Data.Set (Set)
import qualified Data.Set as Set

-- x function
x :: Integer -> Integer
x n = (mod (1248^n) 32323) - 16161

-- y function
y :: Integer -> Integer
y n = (mod (8421^n) 30103) - 15051

-- generate tuples of (xi, yi)
generateTuple :: Integer -> (Integer, Integer)
generateTuple n = (x n, y n)

-- p function = {(x1, y1), (x2, y2),...,(xn, yn)}
p :: Integer -> Set.Set (Integer, Integer)
p n = Set.fromList (map generateTuple [1..n])

-- finds all the possible triads from all the points in p n
generateCombinations :: Set.Set (Integer, Integer) -> Int -> [Set.Set (Integer, Integer)]
generateCombinations tupleSet size = 
    let tupleList = Set.toList tupleSet
    in [Set.fromList combination | combination <- subsequences tupleList, length combination == size]

-- Given three points A(x1, y1), B(x2, y2), and C(x3, y3), you can calculate the area of the triangle formed by these points using the determinant method:
-- Area = 0.5 * |x1(y2 - y3) + x2(y3 - y1) + x3(y1 - y2)|
-- Calculate the area using the formula.
-- If the area is greater than 0, it means the origin is outside the triangle.
-- If the area is less than 0, it means the origin is inside the triangle.
formsTriangle :: [(Integer, Integer)] -> Bool
formsTriangle tupleList =
    case tupleList of
        [(x1, y1), (x2, y2), (x3, y3)] ->
            -- Check if the origin (0, 0) is inside the triangle formed by the points
            let area = 0.5 * fromInteger (abs (x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2)))
            in area < 0
        _ -> False

-- find all permutations of each combination
--   eg permutations(fromList [(8340,-10778),(12407,1060),(15852,-5203)]) =
--   fromList [(8340,-10778),(12407,1060),(15852,-5203)]
--   fromList [(8340,-10778),(15852,-5203),(12407,1060)]
--   fromList [(12407,1060),(8340,-10778),(15852,-5203)]
--   fromList [(12407,1060),(15852,-5203),(8340,-10778)]
--   fromList [(15852,-5203),(8340,-10778),(12407,1060)]
--   fromList [(15852,-5203),(12407,1060),(8340,-10778)]
-- check if each permutation forms a triangle
-- count the triangles
countTrianglesContainingOrigin :: [Set.Set (Integer, Integer)] -> Int
countTrianglesContainingOrigin combinationsList =
    sum [length [p | p <- permutations (Set.toList combo), formsTriangle p] | combo <- combinationsList]

-- c function -> counts number of triangles with vertices in p n that contain the origin O(0, 0) 
-- for this, we need to find first all the possible combinations of points, 
-- so we find all the possible triads from all the p n
c :: Integer -> Int
c n = countTrianglesContainingOrigin (generateCombinations (p n) 3)

main :: IO ()
main = do
    -- let combinationsList = generateCombinations (p 8) 3
    -- mapM_ print combinationsList
    let count = c 8
    putStrLn $ "Number of combinations forming triangles containing the origin: " ++ show count
