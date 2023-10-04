module Exercise3 where
    
-- ## Task ##
-- Implement a function that calculates the minimal property subsets, 
-- given a 'function under test' and a set of properties

-- ## Deliverables ##
-- implementation, documentation of approach, indication of time spent
-- Time spent: 180 min

-- ## Documentation of Approach ##
-- We take as an argument something comparabable and generic (need to work with constaints)
-- Need to derive if a property is a subset of another property, if yes replace it (recursion maybe)
-- Use stronger / weaker functions and leave in the end only the strongest ones or equal and this is the subset (?)

calculateMinimalSubset :: (a -> b) -> [Bool] -> [c] 
calculateMinimalSubset _ [] = [] -- if properties set is empty, there can't be a minimal set 
calculateMinimalSubset fut (x:xs) = undefined 