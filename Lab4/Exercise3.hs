module Exercise3 where

import Data.List 
-- ## Deliverables 
-- Haskell program, indication of time spent

-- ## Indication of type spent
-- Total: 15 minutes (5 minutes implementation + 10 min documentation)

-- Type was taken from the *.pdf in the lab. 
type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos relation = sort $ nub $ concat [ [(x,y),(y,x)] | (x,y) <- relation] 

-- ## Discussion

-- ### Definition of Symmetric Closure 

-- Basis for this function was the definition on pg. 166 in the Haskell Road
-- to Logic book . A relation R on a set A is symmetric if for all x,y ∈ A; if xRy then yRx.

-- ### "Different" Implementations

-- 1.) This was the first, intuitive implementation of the symClos. 
-- I used list comprehension because it seemed intuitive to me with a first thought. 
-- I create a "nested" list with inversed pairs, concat it to make it into one
-- list and then sort it (because sets need to be sorted).
-- "Advantage": Was the first solution that came to my mind 
-- Disadvantage: It creates a unnecessary nested list which can be bypassed with 
-- the next implementation approaches. 

-- 2.) Another possible implementation would be to create an "inverse" helper-function 
-- which basically does the same as the list comprehension.
-- Advantage: the code is more "beautiful" (which is subjective) and does not contain
-- unnecessary "conversions" as in implementation 1.

-- inverse :: Rel a -> Rel a
-- inverse = map (\ (x,y) -> (y,x)) 

-- symClos :: Ord a => Rel a -> Rel a
-- symClos r = sort ( nub (r ++ inverse r) )

-- You could replace the "inverse" function with the Data.Tuple "swap" function 
-- And there are probably many other ways to make this more beautiful.

-- 3.) Another possible implementation taken from the Haskell Road to Logic Book 
-- pg. 198 
-- Create the union of the initial relation and the inverse of the relation
-- (unionR' and symClosure') 


-- ### Testing
-- Testing is provided in Exercise 5 based on a bunch of properties. 