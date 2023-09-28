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

iotraces :: IOLTS -> [[Label]]
iotraces (q, li, lo, lt, q0) = nub $ map snd (traces' lt [([q0],[])])

infixr 5 `after`  
after :: IOLTS -> Trace -> [State] 
after (_, _, _, [], _) _ = []  -- no transitions, return empty states
after (_,_,_,_,q0) [] = [q0] -- epsilon state in the beginning
after (_, _, _, transitions, initialState) trace =
    findFollowingStates initialState trace transitions

-- Function to find following states based on the trace
findFollowingStates :: State -> Trace -> [LabeledTransition] -> [State]
findFollowingStates currentState [] _ = [currentState]
findFollowingStates currentState (label:rest) transitions =
    let matchingTransitions = filter (\(_, l, _) -> l == label) transitions
        nextStates = map (\(_, _, nextState) -> nextState) matchingTransitions
    in
        concatMap (\nextState -> findFollowingStates nextState rest transitions) nextStates

