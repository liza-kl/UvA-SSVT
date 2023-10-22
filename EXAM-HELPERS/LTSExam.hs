module LTSExam where

import Data.List
import LTSLecture
import Test.QuickCheck

-- GENERATORS FOR LTS


{-- A separate generator for the state, just numbers. Could have a bigger range--}
ltsGenState :: Gen State -- A generator for a single state
ltsGenState = elements [0..5]

{-- A separate generator for the labels, just chars. Could go further, or generate random chars--}
ltsGenLabel :: Gen Label
ltsGenLabel = elements [ return x | x <- ['a'..'e']] 

{-- A separate generator for the labeled transitions --}
ltsGenLabeledTransition :: Gen State -> Gen Label -> Gen LabeledTransition
ltsGenLabeledTransition possibleStateGen possibleLabelGen = do
    firstState <- possibleStateGen
    transitionLabel <- possibleLabelGen
    lastState <- possibleStateGen
    return (firstState, transitionLabel, if transitionLabel == delta then firstState else lastState)

-- this generator generates an IOLTS without delta states
{-- Bringing all the above components together --}
ltsGen :: Gen IOLTS
ltsGen = do
    setOfPossibleStates <- listOf ltsGenState `suchThat` (not . null . nub)
    initialStateIndex <- choose (0, length setOfPossibleStates - 1)
    let initialState = setOfPossibleStates !! initialStateIndex
    setOfPossibleInputs <- nub <$> listOf ltsGenLabel -- nub, because elements in a set need to be unique 
    setOfPossibleOutputs <- filter (`notElem` setOfPossibleInputs) . nub <$> listOf ltsGenLabel
    setOfPossibleLabeledTransitions <- nub <$> listOf (ltsGenLabeledTransition (elements setOfPossibleStates) (elements (setOfPossibleInputs ++ [tau])))
    -- According to the definition we need to include tau in the inputs
    return (nub setOfPossibleStates, setOfPossibleInputs, setOfPossibleOutputs, setOfPossibleLabeledTransitions, initialState)

-- this generator generates an IOLTS with delta states
ltsGenDelta :: Gen IOLTS
ltsGenDelta = do
    setOfPossibleStates <- listOf ltsGenState `suchThat` (not . null . nub)
    initialStateIndex <- choose (0, length setOfPossibleStates - 1)
    let initialState = setOfPossibleStates !! initialStateIndex
    setOfPossibleInputs <- nub <$> listOf ltsGenLabel
    setOfPossibleOutputs <- filter (`notElem` setOfPossibleInputs) . nub <$> listOf ltsGenLabel
    setOfPossibleLabeledTransitions <- nub <$> listOf (ltsGenLabeledTransition (elements setOfPossibleStates) (elements (setOfPossibleInputs ++ [tau] ++ [delta])))
    -- Recording to the Definition we need to include tau in the inputs
    return (nub setOfPossibleStates, setOfPossibleInputs, setOfPossibleOutputs ++ [delta], setOfPossibleLabeledTransitions, initialState)


{-- Solution for Exercise 4--}


-- LTS Stuff 

coffeeImpl :: LTS
coffeeImpl = ([1..4], ["?btn", "?btn_milk", "!espresso", "!latte"], [(1, "?btn", 2), (1, "?btn_milk", 1), (2, "!espresso", 3), (2, "!latte", 4)], 1)

coffeeModel :: LTS
coffeeModel = ([1..5], ["?btn", "?btn_milk", "!espresso", "!latte"],[(1, "?btn", 2), (2, "!espresso", 3), (1, "?btn_milk", 4), (4,"!latte", 5)], 1)

ourContains:: Eq a => [a] -> a -> Bool
ourContains [] _ = False
ourContains (x:xs) elemToCheck
    | x == elemToCheck = True
    | otherwise = ourContains xs elemToCheck




out :: IOLTS -> [Label] -> [Label]
out (states, labelsI, labelsO, transitions, init) sigma = noRepetitions
    where
    noRepetitions    = removeDuplicateLabelsTau findingAllLabels sigma
    findingAllLabels = findLabelsForState stateSet transitions
    -- using the states from the after function defined in ex 4, and getting the labels
    -- achievable from transitions originating from such states
    stateSet         = after iolts sigma
    iolts            = (states, labelsI, labelsO, transitions, init)


findLabelsForState :: [State] -> [LabeledTransition] -> [Label]
findLabelsForState _ [] = []
findLabelsForState listStates ((s1, label, s2) : transitions)
    | s1 `elem` listStates = label : findLabelsForState listStates transitions
    | otherwise            = findLabelsForState listStates transitions

    -- Removes the labels that were in the sigma list (list of labels we want to check) and also
    -- all the tau labels.
    -- This is according to Chapter 4.1 of Tretman's paper.

removeDuplicateLabelsTau :: [Label] -> [Label] -> [Label]
removeDuplicateLabelsTau [] _ = []
removeDuplicateLabelsTau (l1 : other) sigma
    | l1 `elem` sigma || l1 == "tau" = removeDuplicateLabelsTau other sigma
    | otherwise                      = l1 : removeDuplicateLabelsTau other sigma

-----


out' :: IOLTS -> [State] -> [Label]
-- case for when the state list is empty, where we return the delta LTS datatype
out'  (states, inputs, outputs, transitions, state) [] = [delta]
out' (_, _, outputs, transitions, _) states =
    filter (`elem` outputs) (nub $ concatMap (map snd . nextTransitions' transitions) states)


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



{-- Exercise 4 --}
-- Will coffee be dispense or not. Basically you are looking for the out function.
-- type IOLTS = ([State], [Label], [Label], [LabeledTransition], State)

traceResult :: Trace -> [Label]
traceResult traceToCheck =
             let coffeeImplStuff = [(1, "?btn", 2), (2, "!espresso", 3), (1, "?btn_milk", 4), (4,"!latte", 5)]
                 coffeeIOLTS = createIOLTS coffeeImplStuff
                 result = out' coffeeIOLTS (coffeeIOLTS `after'` traceToCheck)
            in result

quantumCoffee :: Trace -> Bool
quantumCoffee traceToCheck =
        let coffeeImplStuff = [(1, "?btn", 2), (2, "!espresso", 3), (1, "?btn_milk", 4), (4,"!latte", 5)]
            coffeeIOLTS = createIOLTS coffeeImplStuff
            result = out' coffeeIOLTS (coffeeIOLTS `after'` traceToCheck)
    in
        ourContains result "latte"

main = do
    -- Define or import traces and coffeeModel
    let myList = traces coffeeModel
    -- You need to provide the 'trace' argument when calling quantumCoffee
    let traceToCheck =  ["?btn_milk"]-- Example trace
    let result = (if quantumCoffee traceToCheck then "Latte dispensed" else "Latte not dispensed")
    print  result