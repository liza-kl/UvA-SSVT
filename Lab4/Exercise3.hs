module Exercise3 where

import Data.List 
-- ## Deliverables 
-- Haskell program, indication of time spent

-- ## Indication of type spent
-- 5 Minutes?! 

-- Type was taken from the *.pdf in the lab. 
type Rel a = [(a,a)]

-- This was the first implementation of the symClos. 
symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos relation = sort $ concat [ [(x,y),(y,x)] | (x,y) <- relation] 

-- Another possible implementation would be to create a inverse helper-function 
-- symClos :: Ord a => Rel a -> Rel a
-- symClos r = sort ( nub (r ++ inverse r) )


-- Provide a discussion about the implementation, different implementation
-- Why is this expected to be so 
-- Relate to this Exercise 5 for the testing 