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

removePrefixChar :: Char -> String -> String
removePrefixChar prefixChar str =
    case stripPrefix [prefixChar] str of
        Just rest -> rest
        Nothing -> str
        
-- Function to find following states based on the trace
findFollowingStates :: State -> Trace -> [LabeledTransition] -> [State]
-- if no transition is made, the iolts remains in the current state 
findFollowingStates currentState [] _ = [currentState] 
-- the input label is important to filter afterwards  
findFollowingStates currentState (input:rest) transitions = 
    -- check for the ? in the inputs, and only consider the ones which have
    -- the corresponding input which is asked for
    let matchingTransitions = filter (\(_, l, _) -> (l == input) || (l == (removePrefixChar '?' input))) transitions
    -- look at the current transition tuple, return the next state 
        nextStates = map (\(_, _, nextState) -> nextState) matchingTransitions 
    in
        -- map over next states, recurse, concat the maps for all the states.
        concatMap (\nextState -> findFollowingStates nextState rest transitions) nextStates

