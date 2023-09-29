module Exercise2 where
import LTS
import Exercise1
import Test.QuickCheck
import Data.List
{-- 
Time spent: 60 Minutes
--}

{-- Thinking process before implementing the Generator --}

{-- type IOLTS = ([State], [Label], [Label], [LabeledTransition], State)

IOLTS consist 

- Set of possible States in the LTS (Q)
- Set of possible Inputs for the LTS (L_I)
- Set of possible outputs for the LTS (L_U)
- Set of possible labeled transitions for the LTS (T)
- An initil State (q) which needs to be in the Q 
(Definition 6 in the paper)

So we need some individual Generators for the components. 
Maybe we could use a predefined lists for the labels?
- Regarding the state the generator only needs to create a random number, in the list they need to be unique 
- Regarding the label: Create a list of random labels? 
-- Quiescence should be considered 
 --}

{-- For sake of ease, transitioned labels need to have at least one elem.--}

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


{-- QuickCheck tests with properties from Exercise1 --}
main :: IO()
main = do
   -- Tests for ltsGen 
   quickCheck $ forAll ltsGen validateLTS
   quickCheck $ forAll ltsGen prop_initialSetNotEmpty
   quickCheck $ forAll ltsGen prop_interSectionOfInputOutputIsEmpty
   quickCheck $ forAll ltsGen prop_tauNotInInputSet
   quickCheck $ forAll ltsGen prop_initialStateInStateSet
   quickCheck $ forAll ltsGen prop_cartesianRelationInTransition
   quickCheck $ forAll ltsGen prop_deltaBehavior
   quickCheck $ forAll ltsGen prop_InputAndOutputIsCountable

   -- Tests for ltsGenDelta
   quickCheck $ forAll ltsGenDelta validateLTS
   quickCheck $ forAll ltsGenDelta prop_initialSetNotEmpty
   quickCheck $ forAll ltsGenDelta prop_interSectionOfInputOutputIsEmpty
   quickCheck $ forAll ltsGenDelta prop_tauNotInInputSet
   quickCheck $ forAll ltsGenDelta prop_initialStateInStateSet
   quickCheck $ forAll ltsGenDelta prop_cartesianRelationInTransition
   quickCheck $ forAll ltsGenDelta prop_deltaBehavior
   quickCheck $ forAll ltsGenDelta prop_InputAndOutputIsCountable