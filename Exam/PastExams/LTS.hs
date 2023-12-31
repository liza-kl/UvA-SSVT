module LTS where
import Data.List

-- Types
type State = Integer
type Label = String
type LabeledTransition = (State, Label, State)
type Trace = [Label]
type LTS = ([State], [Label], [LabeledTransition], State)
type IOLTS = ([State], [Label], [Label], [LabeledTransition], State)

-- Symbols
tau :: Label
tau = "tau"

delta :: Label
delta = "delta"

-- IOLTS's from Axini presentation
counterImpl :: IOLTS
counterImpl = createIOLTS [(1, "?coin", 2), (2, "!tea", 3), (2, "!coffee", 4)]
counterModel :: IOLTS
counterModel = createIOLTS [(1, "?coin", 2), (1, "?coin", 3), (2, "!tea", 4), (3, "!coffee", 5)]

coffeeImplSimple :: LTS
coffeeImplSimple = createLTS [(1, "coin", 2), (2, "coffee", 3)]

coffeeImpl1 :: IOLTS
coffeeImpl1 = createIOLTS [(1, "?coin", 2), (2, "!coffee", 3)]
coffeeModel1 :: IOLTS
coffeeModel1 = createIOLTS [(1, "?coin", 2), (2, "!tea", 3), (2, "!coffee", 4)]

coffeeModel2 :: IOLTS
coffeeModel2 = createIOLTS [(1, "?coin", 2), (2, "!coffee", 3)]
coffeeImpl2 :: IOLTS
coffeeImpl2 = createIOLTS [(1, "?coin", 2), (2, "!tea", 3), (2, "!coffee", 4)]

coffeeImpl3 :: IOLTS
coffeeImpl3 = createIOLTS [(1, "?coin", 2), (2, "!coffee", 3), (1, "?button", 2), (1, "?tea", 2)]
coffeeModel3 :: IOLTS
coffeeModel3 = createIOLTS [(1, "?coin", 2), (2, "!coffee", 3)]

coffeeImpl4 :: IOLTS
coffeeImpl4 = createIOLTS [(1, "?coin", 2), (2, "!coffee", 3)]
coffeeModel4 :: IOLTS
coffeeModel4 = createIOLTS [(1, "?button", 2), (2, "!tea", 3), (1, "?coin", 4), (4, "!coffee", 5)]

coffeeImpl5 :: IOLTS
coffeeImpl5 = createIOLTS [(1, "?coin", 2), (2, "!coffee", 3), (1, "?coin", 4)]
coffeeModel5 :: IOLTS
coffeeModel5 = createIOLTS [(1, "?coin", 2), (2, "!coffee", 3)]

coffeeImpl6 :: IOLTS
coffeeImpl6 = createIOLTS [(1, "?coin", 2), (1, "?coin", 3), (3, "!coffee", 4)]
coffeeModel6 :: IOLTS
coffeeModel6 = createIOLTS [(1, "?coin", 2), (2, tau, 3), (2, "!coffee", 3)]

-- LTS's from Tretmans paper
tretmanP :: LTS
tretmanP = createLTS [(0, "but", 1), (1, "liq", 2)]

tretmanQ :: LTS
tretmanQ = createLTS [(0, "but", 1), (1, "liq", 2), (1, "choc", 3)]

tretmanR :: LTS
tretmanR = createLTS [(0, "but", 1), (0, "but", 2), (1, "liq", 3), (2, "but", 4), (4, "choc", 5)]

tretmanU :: LTS
tretmanU = createLTS [(0, "but", 1), (1, "liq", 0), (1, "choc", 0)]

tretmanV :: LTS
tretmanV = createLTS [(0, "but", 1), (1, "liq", 0), (1, tau, 0)]

tretmanK1 :: IOLTS
tretmanK1 = createIOLTS [(0, "?but", 1), (1, "?but", 1), (1, "!liq", 2), (2, "?but", 2)]

tretmanK2 :: IOLTS
tretmanK2 = createIOLTS [(0, "?but", 1), (1, "?but", 1), (1, "!liq", 2), (2, "?but", 2), (1, "!choc", 3), (3, "?but", 3)]

tretmanK3 :: IOLTS
tretmanK3 = createIOLTS [(0, "?but", 1), (1, "?but", 1), (0, "?but", 2), (1, "!liq", 3), (2, "?but", 4), (4, "?but", 4), (4, "!choc", 5), (5, "?but", 5)]

tretmanI1 :: IOLTS
tretmanI1 = createIOLTS [(0, "?b", 0), (0, "?a", 1), (1, "?a", 1), (1, "?b", 1), (1, "!x", 2), (2, "?a", 2), (2, "?b", 2)]

tretmanI2 :: IOLTS
tretmanI2 = createIOLTS [(0, "?b", 0), (0, "?a", 1), (1, "?a", 1), (1, "?b", 1), (1, "!x", 2), (2, "?a", 2), (2, "?b", 2), (1, "!y", 3), (3, "?a", 3), (3, "?b", 3)]

tretmanI3 :: IOLTS
tretmanI3 = createIOLTS [(0, "?a", 1), (1, "?a", 1), (1, "?b", 1), (1, "!x", 2), (2, "?a", 2), (2, "?b", 2), (0, "?b", 3), (3, "?a", 3), (3, "?b", 3), (3, "!y", 4), (4, "?a", 4), (4, "?b", 4)]

tretmanI4 :: IOLTS
tretmanI4 = createIOLTS [(0, "?b", 0), (0, "?a", 1), (1, "?a", 1), (1, "?b", 1), (0, "?a", 2), (2, "?a", 2), (2, "?b", 2), (2, "!x", 3), (3, "?a", 3), (3, "?b", 3)]

tretmanS1 :: IOLTS
tretmanS1 = createIOLTS [(0, "?a", 1), (1, "!x", 2)]

tretmanS2 :: IOLTS
tretmanS2 = createIOLTS [(0, "?a", 1), (1, "!x", 2), (1, "!y", 3)]

tretmanS3 :: IOLTS
tretmanS3 = createIOLTS [(0, "?a", 1), (1, "!x", 2), (0, "?b", 3), (3, "!y", 4)]

tretmanS4 :: IOLTS
tretmanS4 = createIOLTS [(0, "?a", 1), (1, "!x", 2), (1, tau, 3)]

tretmanR1 :: IOLTS
tretmanR1 = createIOLTS [(0, "?but", 1), (1, "?but", 1), (0, "?but", 2), (1, "!liq", 3), (2, "?but", 4), (4, "?but", 4), (4, "!choc", 5), (5, "?but", 5), (4, "!lic", 6), (6, "?but", 6)]

tretmanR2 :: IOLTS
tretmanR2 = createIOLTS [(0, "?but", 1), (1, "?but", 1), (0, "?but", 2), (1, "!liq", 3), (2, "?but", 4), (4, "?but", 4), (4, "!choc", 5), (5, "?but", 5)]

-- Converts an LTS into an IOLTS
createIOLTS :: [LabeledTransition] -> IOLTS
createIOLTS transitions = (states, map tail $ filter (\x -> head x == '?') labels, map tail $ filter (\x -> head x == '!') labels, map (\(f, l, t) -> (f, makeLabel l, t)) transitionSet, initState)
      where (states, labels, transitionSet, initState) = createLTS transitions

makeLabel :: Label -> Label
makeLabel x | fst == '?' || fst == '!' = tail x
            | otherwise = x
      where fst = head x

-- Creates an LTS from a list of transitions. Assumes that this list describes all states and labels, and that the lowest numbered state is the initial state.
createLTS :: [LabeledTransition] -> LTS
createLTS transitions = (states, filter (/= tau) $ makeSet (map (\(_,label,_) -> label) transitions), makeSet transitions, head states)
      where states = makeSet (concatMap (\(from,_,to) -> [from, to]) transitions)

makeSet :: Ord a => [a] -> [a]
makeSet = sort . nub

-- Door implementations
doorImpl1 :: State -> Label -> (State, Label)
doorImpl1 0 "close" = (1, "closed")
doorImpl1 1 "open" = (0, "opened")
doorImpl1 1 "lock" = (2, "locked")
doorImpl1 2 "unlock" = (1, "unlocked")
doorImpl1 _ _ = error "Invalid command and/or state!"

doorImpl2 :: State -> Label -> (State, Label)
doorImpl2 0 "close" = (1, "opened")
doorImpl2 1 "open" = (0, "closed")
doorImpl2 1 "lock" = (2, "locked")
doorImpl2 2 "unlock" = (1, "unlocked")
doorImpl2 _ _ = error "Invalid command and/or state!"

doorImpl3 :: State -> Label -> (State, Label)
doorImpl3 0 "close" = (1, "closed")
doorImpl3 1 "open" = (0, "opened")
doorImpl3 1 "lock" = (2, "locked")
doorImpl3 2 "unlock" = (2, "unlocked")
doorImpl3 _ _ = error "Invalid command and/or state!"

doorImpl4 :: State -> Label -> (State, Label)
doorImpl4 0 "close" = (1, "closed")
doorImpl4 1 "open" = (0, "opened")
doorImpl4 1 "unlock" = (2, "locked")
doorImpl4 2 "lock" = (1, "unlocked")
doorImpl4 _ _ = error "Invalid command and/or state!"

doorImpl5 :: State -> Label -> (State, Label)
doorImpl5 0 "close" = (1, "closed")
doorImpl5 1 "open" = (0, "open")
doorImpl5 1 "lock" = (2, "locked")
doorImpl5 2 "unlock" = (1, "unlocked")
doorImpl5 _ _ = error "Invalid command and/or state!"

doorImpl6 :: State -> Label -> (State, Label)
doorImpl6 0 "close" = (1, "closed")
doorImpl6 1 "open" = (3, "opened")
doorImpl6 3 "close" = (4, "closed")
doorImpl6 4 "open" = (5, "opened")
doorImpl6 5 "close" = (6, "closed")
doorImpl6 6 "open" = error "Door is stuck!"
doorImpl6 1 "lock" = (2, "locked")
doorImpl6 4 "lock" = (2, "locked")
doorImpl6 6 "lock" = (2, "locked")
doorImpl6 2 "unlock" = (1, "unlocked")
doorImpl6 _ _ = error "Invalid command and/or state!"

doorImpl7 :: State -> Label -> (State, Label)
doorImpl7 0 "close" = (1, "closed")
doorImpl7 1 "open" = (0, "opened")
doorImpl7 1 "lock" = (2, "locked")
doorImpl7 2 "unlock" = (3, "unlocked")
doorImpl7 4 "close" = (5, "closed")
doorImpl7 3 "open" = (4, "opened")
doorImpl7 3 "lock" = (2, "locked")
doorImpl7 5 "open" = (0, "opened")
doorImpl7 5 "lock" = (6, "locked")
doorImpl7 6 "unlock" = error "Incorrect keycode!"
doorImpl7 _ _ = error "Invalid command and/or state!"

doorImpl8 :: State -> Label -> (State, Label)
doorImpl8 0 "close" = (1, "closed")
doorImpl8 1 "open" = (0, "opened")
doorImpl8 1 "lock" = (2, "locked")
doorImpl8 2 "unlock" = (3, "unlocked")
doorImpl8 4 "close" = (5, "closed")
doorImpl8 3 "open" = (4, "opened")
doorImpl8 3 "lock" = (2, "locked")
doorImpl8 5 "open" = (6, "opened")
doorImpl8 5 "lock" = (2, "locked")
doorImpl8 6 "close" = (7, "closed")
doorImpl8 7 "lock" = (2, "locked")
doorImpl8 7 "close" = (2, "closed")
doorImpl8 _ _ = error "Invalid command and/or state!"

nextTransitions':: [LabeledTransition]->State->[(State,Label)]
nextTransitions' lt q0 =  [(s',l) | (s,l,s')<- lt , s == q0]

findfollowingtransitions':: [LabeledTransition] -> [State] -> [Label] -> [([State],[Label])]
findfollowingtransitions' lt st ls = [(s:st,ls++[l])| (s,l)<-nextTransitions' lt (head st)]

traces':: [LabeledTransition] -> [([State],[Label])]-> [([State],[Label])]
traces' lt [] = []
traces' lt pairs = pairs ++ traces' lt next
    where next = concatMap (uncurry $ findfollowingtransitions' lt) pairs

traces :: LTS -> [Trace] -- [[Label]]
traces (q, l, lt, q0) = nub $ map snd (traces' lt [([q0],[])])


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

