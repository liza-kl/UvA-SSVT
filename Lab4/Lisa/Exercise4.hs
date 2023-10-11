module Exercise4 where

-- A relation R is serial on a domain A if for any x ∈ A there is an y ∈ A 
-- such that xRy. Suppose relations are represented as lists of pairs
-- Thought of process: First understand what they want from me.
-- But you are basically providing a Set of "a" -- numbers and check if there is 
-- a corresponding y in the thing. Serial Relation must be binary 
-- Time spent: until now 10 Minutes ?!
-- Example of a serial relation is less then 
type Rel a = [(a,a)]

isSerial :: Eq a => [a] -> [(a, a)] -> Bool
isSerial [] _ = True -- Base case: If we found a counterpart for every value in the relation, stop.
isSerial (x:xs) relation
    | any (\(a, b) -> a == x && b `elem` map snd relation) relation = isSerial xs relation
    | otherwise = False
