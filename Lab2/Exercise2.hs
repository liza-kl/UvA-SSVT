module Exercise2 where
import LTS
import Exercise1
import Test.QuickCheck
import Data.List
{-- 
Time spent: 60 Minutes
--}

{-- Thinking process before implementing the Generator
type IOLTS = ([State], [Label], [Label], [LabeledTransition], State)
The type IOLTS consists of a 

- Set of possible States in the LTS (Q)
- Set of possible Inputs for the LTS (L_I)
- Set of possible outputs for the LTS (L_U)
- Set of possible labeled transitions for the LTS (T)
- An initil State (q) which needs to be in the Q 
(Definition 6 in the paper)

So we need some individual Generators for the components. Maybe we could use a predefined lists for 
the labels?
- Regarding the state the generator only needs to create a random number, in the list they need to be unique 
-- Do the numbers have to be 
- Regarding the label: Create a list of random labels? 
-- Epsilon States need to be considered and the quiescent stuff
 --}


ltsGenState :: Gen State -- A generator for a single state
ltsGenState = elements [0..15]

ltsGenLabel :: Gen Label -- A generator for a single label
ltsGenLabel = elements [ return x | x <- ['a'..'z']]

ltsGenLabeledTransition :: Gen State -> Gen Label -> Gen LabeledTransition
ltsGenLabeledTransition possibleStateGen possibleLabelGen = do
    firstState <- possibleStateGen
    transitionLabel <- possibleLabelGen
    lastState <- possibleStateGen
    return (firstState, transitionLabel, lastState)

ltsGen :: Gen IOLTS
ltsGen = do
    setOfPossibleStates <- listOf ltsGenState `suchThat` (not . null . nub)
    initialStateIndex <- choose (0, length setOfPossibleStates - 1)
    let initialState = setOfPossibleStates !! initialStateIndex
    setOfPossibleInputs <- nub <$> listOf ltsGenLabel
    setOfPossibleOutputs <- filter (`notElem` setOfPossibleInputs) . nub <$> listOf ltsGenLabel
    setOfPossibleLabeledTransitions <- nub <$> listOf (ltsGenLabeledTransition (elements setOfPossibleStates) (elements (setOfPossibleInputs ++ [tau])))
    -- Recording to the Definition we need to include tau in the inputs
    return (setOfPossibleStates, setOfPossibleInputs, setOfPossibleOutputs, setOfPossibleLabeledTransitions, initialState)


{-- QuickCheck tests with properties from Exercise1 --}
main :: IO()
main = do
   -- Tests for ltsGen 
   quickCheck $ forAll ltsGen prop_initialSetNotEmpty
   quickCheck $ forAll ltsGen prop_interSectionOfInputOutputIsEmpty
   quickCheck $ forAll ltsGen prop_tauNotInInputSet
   quickCheck $ forAll ltsGen prop_initialStateInStateSet
   quickCheck $ forAll ltsGen prop_cartesianRelationInTransition


