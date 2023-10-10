module Exercise3 where

-- ## Deliverables 
-- Haskell program, indication of time spent
-- ## Indication of type spent
-- 5 Minutes?! 

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos relation = concat [ [(x,y),(y,x)] | (x,y) <- relation] 

