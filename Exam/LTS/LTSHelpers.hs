module LTSHelpers where 

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

-- Taken from : https://github.com/AndreasElgaard/SSVT-Exercises/blob/main/Lab%204/Exercise4.hs

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

findFinalStateFromLabel :: Label -> [LabeledTransition] -> [State]
findFinalStateFromLabel _ [] = []
findFinalStateFromLabel l1 ((s1, label, s2) : transitions)
    | l1 == label = s2 : findFinalStateFromLabel l1 transitions
    | otherwise                       = [] 

findTaus :: [LabeledTransition] -> [State]-> [State]
findTaus [] _ = []
findTaus  ((s1, label, s2) : transitions) stat
    | label == "tau" && s1 `elem` stat = s2 : findTaus transitions stat
    | otherwise                       = findTaus transitions stat


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

