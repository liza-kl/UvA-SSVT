module Euler456 where
import Test.QuickCheck
import Data.List 
import System.Random
import Data.Set (Set)
import qualified Data.Set as Set


-- Time spent: 120 min
-- The program works but for large numbers such as 2,000,000 it is not performant at all
-- so it takes a lot of time to get a result

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
p :: Integer -> Set (Integer, Integer)
p n = Set.fromList (map generateTuple [1..n])

-- finds all the possible triads from all the points in p n
generateCombinations :: Set (Integer, Integer) -> Int -> [Set (Integer, Integer)]
generateCombinations tupleSet size = 
    let tupleList = Set.toList tupleSet
    in [Set.fromList combination | combination <- subsequences tupleList, length combination == size]

-- Given three points A(x1, y1), B(x2, y2), and C(x3, y3), 
-- we can calculate the area of the triangle (shoelace formula)
-- and the areas of the triangles that get formed by two of these points
-- and the point that we want to see if it is contained in the initial triangle
-- then we check if the sum of the small areas is equal to the area of the whole triangle
-- if it is equal, that means that the point is contained in the triangle
formsTriangle :: [(Integer, Integer)] -> Bool
formsTriangle tupleList =
    case tupleList of
        [(x1, y1), (x2, y2), (x3, y3)] ->
            -- area of ABC
            let a = 0.5 * fromInteger (abs (x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2)))
            -- area of PBC
                a1 = 0.5 * fromInteger (abs (0 * (y2 - y3) + x2 * (y3 - 0) + x3 * (0 - y2)))
            -- area of PAC
                a2 = 0.5 * fromInteger (abs (x1 * (0 - y3) + 0 * (y3 - y1) + x3 * (y1 - 0)))
            -- area of PAB
                a3 = 0.5 * fromInteger (abs (x1 * (y2 - 0) + x2 * (0 - y1) + 0 * (y1 - y2)))
            -- check if sum of a1, a2, a3 is equal to a
            in a == a1 + a2 + a3
        _ -> False

-- for each combination, check if its points form a triangle that contains the origin
-- count all these triangles     
countTrianglesContainingOrigin :: [Set (Integer, Integer)] -> Int
countTrianglesContainingOrigin combinationsList =
    sum [if formsTriangle (Set.toList combination) then 1 else 0 | combination <- combinationsList]


-- c function -> counts number of triangles with vertices in p n that contain the origin O(0, 0) 
-- for this, we need to find first all the possible combinations of points, 
-- so we find all the possible triads from all the p n
c :: Integer -> Int
c n = countTrianglesContainingOrigin (generateCombinations (p n) 3)

main :: IO ()
main = do
    -- let combinationsList = generateCombinations (p 8) 3
    -- mapM_ print combinationsList
    let count = c 2000000
    putStrLn $ "Number of combinations forming triangles containing the origin: " ++ show count