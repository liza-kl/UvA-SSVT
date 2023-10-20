module OutImpl where 
import AfterImpl
import LTS 
import Data.List

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
