module Exercise2 where
import LTS
import Test.QuickCheck
{-- 
Random IOLTS generator(s), QuickCheck tests for validateLts, indication of
time spent
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


{-- Redudant code expressions are for readibility as we are no pros in Haskell </3 --}
ltsGenState :: Gen State -- A generator for a single state
ltsGenState = do
    elements [0..3]

ltsGenLabel :: Gen Label -- A generator for a single label
ltsGenLabel = do
    elements ["OneState", "SecondState", "ThirdState", "FourthState"]

ltsGenLabeledTransition :: Gen State -> Gen Label -> Gen LabeledTransition
ltsGenLabeledTransition possibleStateGen possibleLabelGen = do
    firstState <- possibleStateGen
    transitionLabel <- possibleLabelGen
    lastState <- possibleStateGen
    return (firstState, transitionLabel, lastState)

ltsGen :: Gen IOLTS
ltsGen = do
    initialState <- ltsGenState
    setOfPossibleStates <- listOf ltsGenState
    setOfPossibleInputs <- listOf ltsGenLabel
    setOfPossibleOutputs <- listOf ltsGenLabel
    setOfPossibleLabeledTransitions <- listOf (ltsGenLabeledTransition (elements setOfPossibleStates) (elements setOfPossibleInputs))
    return (setOfPossibleStates, setOfPossibleInputs, setOfPossibleOutputs, setOfPossibleLabeledTransitions, initialState)

main :: IO () 
main = do 
    someStuff <- generate ltsGen
    print someStuff
