module Exercise4 where

import LTS
import Data.List
{-- 
Haskell program, tests, short test report, indication of time spent
Indication of time spent: 60 Minutes

--}

-- out is the output with the ! mark 


-- quescient state in after should be the same state as before
-- Definition number 5 in Tretman 
-- encountering tao you are in both states
-- encountering delta you are looping and staying in same state 
-- after is the state in which you remain 

-- Define the "after" function
after :: IOLTS -> Trace -> [State]
after (_, _, _, [], _) _ = []  -- No labeled transitions, return an empty list
after (_, _, _, transitions, initialState) trace =
    findNextStates initialState trace transitions

-- Function to remove the ?, ! from the input, output
removePrefixChar :: String -> String
removePrefixChar (x:xs)
    | head (x:xs) == '?' || head (x:xs) == '!' = xs
    | otherwise = x:xs

-- Function to find following states based on the trace
findNextStates :: State -> Trace -> [LabeledTransition] -> [State]
-- if no transition is made, the iolts remains in the current state 
findNextStates current [] _ = [current]
-- the input label is important to filter afterwards  
findNextStates current (input:rest) transitions =
    -- check for the ? in the inputs, and only consider the ones which have
    -- the corresponding input which is asked for
    let matchingTransitions = filter (\(_, l, _) -> l == input || l == removePrefixChar input) transitions
    -- look at the current transition tuple, return the next state 
        nextStates = map (\(_, _, nextState) -> nextState) matchingTransitions
    in
        -- map over next states, recurse, concat the maps for all the states.
        -- using nub to have individual results.
        nub (concatMap (\nextState -> findNextStates nextState rest transitions) nextStates)

{-- Tests --}
-- Check where you are after a trace
-- use the state 
main :: IO()
main =
    do
        print "Some tests using the given implementations"
        print (tretmanR2 `after` ["?but"])
        print (tretmanS1 `after` ["?a"])
        print (tretmanI3 `after` ["?a", "?a"])


{-- Short Test Report --}
{-- While testing, bug was found that the state sets are not unique. So we added nub at the end --}