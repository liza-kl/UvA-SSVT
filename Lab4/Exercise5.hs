module Exercise5 where
import Test.QuickCheck
import Data.List
import System.Random
import SetOrd

-- TODO Add discussion between the two implementations (Warshall and Nefeli + 
-- TODO how we did it and what is a transitive closure, definition and how our ).
-- TODO function does this. Maybe also add fixpoint implementation (look slides in course)

-- Indication of time spent: 30 minutes

-- ## Definition of Transitive Clouse
-- Arelation R on A is transitive if for all x;y;z if xRy and yRz then xRz.
type Rel a = [(a,a)]

-- ## Given Helperfunction 
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

trClosFix = fix oneStep 


main :: IO ()
main = do
    let relation = [(1,1),(1,1),(2,2),(2,2),(3,4),(4,3)]
    let transitiveClosure = trClos relation
    print transitiveClosure


-- Maybe it's basically the same, idk . 
-- Compute the full transitive closure using Warshall's algorithm
-- trClos :: Ord a => Rel a -> Rel a
-- trClos relation = closure relation
--   where
--     closure r
--       | r == r2 = r
--       | otherwise = closure r2
--       where r2 = nub (r ++ (r @@ r))