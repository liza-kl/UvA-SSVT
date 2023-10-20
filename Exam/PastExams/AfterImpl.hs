
module AfterImpl where

import           LTS
import           Test.QuickCheck
import Data.List

after :: IOLTS -> [Label] -> [State]
after (states, labelsI, labelsO, transitions, init) sigma 
    | null sigma = [init]
    | null final = []
    | otherwise = final
 where 
    final = tausFinalState ++ last afterImplementation
    tausFinalState = findTaus transitions (last afterImplementation)
    afterImplementation = findAfter [init] sigma transitions
 
findAfter :: [State] -> [Label] -> [LabeledTransition] -> [[State]]
findAfter _ [] _ = []

findAfter s (l1 : otherLabels) trans
    | null findingFirstTransitionsSet = []
    | not (null  findingFirstSet)  = findingFirstSet : findAfter newState otherLabels trans
    | otherwise = []
  where
    findingFirstTransitionsSet = findTransitionsWithInitState (head s) trans
    findingFirstSet = findFinalStateFromLabel l1 findingFirstTransitionsSet
    newState = findingFirstSet

findTransitionsWithInitState:: State -> [LabeledTransition] -> [LabeledTransition]
findTransitionsWithInitState _ [] = []
findTransitionsWithInitState init ((s1, label, s2) : transitions)
    | init == s1 = (s1, label, s2) : findTransitionsWithInitState init transitions
    | init /= s1 && null transitions = []
    | otherwise = findTransitionsWithInitState init transitions

-- We return the final state of every transition that has as a label the one we input in the fucntion.
findFinalStateFromLabel :: Label -> [LabeledTransition] -> [State]
findFinalStateFromLabel _ [] = []
findFinalStateFromLabel l1 ((s1, label, s2) : transitions)
    | l1 == label = s2 : findFinalStateFromLabel l1 transitions
    | otherwise                       = [] 

-- From a list of transitions, finds the ones whose label is tau and their initial state is part 
-- of the found list of states who partake in the trace of the sigma list.
findTaus :: [LabeledTransition] -> [State]-> [State]
findTaus [] _ = []
findTaus  ((s1, label, s2) : transitions) stat
    | label == "tau" && s1 `elem` stat = s2 : findTaus transitions stat
    | otherwise                       = findTaus transitions stat


----

-- Define the "after" function
after' :: IOLTS -> Trace -> [State]
after' (_, _, _, [], _) _ = []  -- No labeled transitions, return an empty list
after' (_, _, _, transitions, initialState) trace =
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
