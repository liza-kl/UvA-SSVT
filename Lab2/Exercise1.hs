
module Exercise1 where
import LTS
import Data.List

{-- 
list of factors, implementation, concise test report, indication of time spent.
--}

{-- 
include tau from lts but not epsilon 
List of Factors:
- 1. The tuple needs to have 5 values (checked bei the iolts type)
- 2. The joint of L_i and L_o must be an empty set (intersection)
- 3. L_i and L_o must be countable (?)
- 4. The initial Q state must not be empty
- 5. The initial q0 state must be in the set of the initial values 
- 6. tau must be in the L_i set
- 7. delta must be a valid input (?) dunno if to include -> ask 

--}

validateLTS :: IOLTS -> Bool
validateLTS ([], _, _, _, _) = False -- if your initial set is empty, it is not an LTS / IOLTS 
validateLTS (setState, inputValues, outputValues, labeledTransitions, initialState) 
    | intersect inputValues outputValues == [] = True 
    | tau `elem` inputValues = True 
    | initialState `elem` setState = True 
    | otherwise = True 

