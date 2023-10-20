module Exercise4 where

import LTS
import Data.List
import Test.QuickCheck 
import Exercise2
{-- 
Haskell program, tests, short test report, indication of time spent
Indication of time spent: 90 Minutes

--}

-- ## Thoughts ##
-- Definition number 5 in Tretman 
-- Encountering tao you are in both states -> kind of "merging"
-- Encountering delta you are looping and staying in same state (Quescience)

-- Define the "after" function
after :: IOLTS -> Trace -> [State]
after (_, _, _, [], _) _ = []  -- No labeled transitions, return an empty list
after (_, _, _, transitions, initialState) trace =
    findNextStates initialState trace transitions

-- Function to remove the ?, ! from the input, output
removePrefixChar :: String -> String
removePrefixChar [] = ""
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


recurseThroughStates :: [State] -> [State] -> Bool
recurseThroughStates [] _ = True
recurseThroughStates (s1:afterList) ioltsList
    | s1 `elem` ioltsList = recurseThroughStates afterList ioltsList
    | otherwise = False


{-- Tests --}
-- Implementation inspired by another team
-- All the states which are in the set of `after` need to be in the IOLTS list of states 
prop_statesExistInTheSet :: IOLTS -> [Label] -> Bool
prop_statesExistInTheSet (states, inputs, outputs, transitions, initialState) labels =
    let iolts = (states, inputs, outputs, transitions, initialState)
        resultAfter = after iolts labels
    in recurseThroughStates resultAfter states

{-- Testing with examples from paper--}
main :: IO()
main =
    do
        print "Some tests using the given implementations"
        print (tretmanR2 `after` ["?but"])
        print (tretmanS1 `after` ["?a"])
        print (tretmanI3 `after` ["?a", "?a"])

        print "QuickCheck Tests"
        quickCheck $ prop_statesExistInTheSet tretmanR2 
        quickCheck $ prop_statesExistInTheSet coffeeImpl1 
        quickCheck $ forAll ltsGen prop_statesExistInTheSet
        quickCheck $ forAll ltsGenDelta prop_statesExistInTheSet





{-- Short Test Report --}
{-- While testing, bug was found that the state sets are not unique. So we added nub at the end --}
{-- Tau and Delta are probably not considered well --}