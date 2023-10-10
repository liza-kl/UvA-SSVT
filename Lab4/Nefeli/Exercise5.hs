module Exercise5 where
import Test.QuickCheck
import Data.List
import System.Random
import SetOrd


-- Indication of time spent: 30 minutes

type Rel a = [(a,a)]

-- get two tuples (x,y) and (w,z)
-- if y == w, then add (x,z) to the result
-- use num to remove duplicates
infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
    nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]
    
-- applies @@ to r with itself,  so we get the tuples after one step
-- then concatenates these results with the initial ones
-- e.g. r = [(1,2),(2,3),(3,4)], r @@ r = [(1,3),(2,4)]
-- r ++ (r @@ r) = [(1,2), (2,3), (3,4), (1,3), (2,4)]
oneStep :: Eq a => Rel a -> Rel a
oneStep r = nub (r ++ (r @@ r))

-- call oneStep until the result is the same as in the previous step
trClos :: Ord a => Rel a -> Rel a
trClos r = until (\x -> x == oneStep x) oneStep r

main :: IO ()
main = do
    let relation = [(1, 2), (2, 3), (3, 4)]
    let transitiveClosure = trClos relation
    print transitiveClosure