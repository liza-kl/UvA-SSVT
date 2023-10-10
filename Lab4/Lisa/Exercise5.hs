{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Exercise5 where

import Data.List
import qualified Data.Set as Set

type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a

-- [(x, y) | (x, y) <- r, (w, z) <- s, y == w]: This is a list comprehension that 
-- iterates through each pair (x, y) in the first relation r and each pair (w, z) 
-- in the second relation s. It checks if the second element of the first pair y 
-- is equal to the first element of the second pair w. If they are equal, 
-- it includes (x, z) in the resulting list.
r @@ s = nub [(x,y) | (x,y) <- r, (w,z) <- s, y == w]

-- Helper Function to create a set out of the tuple list
trClos' relation = nub $ [x | (x, _) <- relation] ++ [y | (_, y) <- relation]

-- https://codereview.stackexchange.com/questions/152190/producing-all-possible-combinations-of-a-list
-- TODO
createGroups :: [a] -> [(a, a)]
createGroups [] = []
createGroups (x:xs) = map ((,) x) xs ++ createGroups xs


trClos :: Ord a => Rel a -> Rel a
trClos relation = createGroups (trClos' relation) @@ relation

--trClos [(1,2),(2,3),(3,4)]  should yield [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]

-- Warshall's algorithm: Number of steps depends on the number of elements in the list 