module Exercise4 where

import LTS 
import Data.List 
{-- 
Haskell program, tests, short test report, indication of time spent
--}

-- out is the output with the ! mark 


-- quescient state in after should be the same state as before
-- Definition number 5 in Tretman 
-- epsilon in a trace should return nothing 
-- Von dem Trace auf die nächsten drübergehen 
-- encountering tao you are in both states
-- encountering delta you are looping and staying in same state 
-- after is the state in which you remain 

-- Define the "after" function
after :: IOLTS -> Trace -> [State]
after (_, _, _, [], _) _ = []  -- No labeled transitions, return an empty list
after (_, _, _, transitions, initialState) trace =
    findFollowingStates initialState trace transitions

-- Function to find following states based on the trace
findFollowingStates :: State -> Trace -> [LabeledTransition] -> [State]
-- if no transition is made, the iolts remains in the current state 
findFollowingStates currentState [] _ = [currentState] 
-- the input label is important to filter afterwards  
findFollowingStates currentState (input:rest) transitions = 
    -- check for the ? in the inputs, and only consider the ones which have
    -- the corresponding input which is asked for
    let matchingTransitions = filter (\(_, l, _) -> (l == input) || (l == "?" ++ input)) transitions
    -- look at the current transition tuple, return the next state 
        nextStates = map (\(_, _, nextState) -> nextState) matchingTransitions 
    in
        -- map over next states, recurse, concat the maps for all the states.
        concatMap (\nextState -> findFollowingStates nextState rest transitions) nextStates

main :: IO ()
main = do
    let iolts = ([1, 2, 3, 4], ["a", "b", "c"], ["x", "y", "z"], [(1, "a", 2), (1,"a",3), (2, "b", 3), (3, "c", 4)], 1)
    let trace = ["a"]
    
    let followingStates = iolts `after` trace
    
    putStrLn "Following States:"
    print followingStates
