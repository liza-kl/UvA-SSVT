

module Exercise6Graphite where
import Data.Graph.DGraph
import Data.Graph.UGraph
import Data.Graph.Visualize
import Data.Graph.Types
import LTS
import Exercise5 (doorImpl5ToIOLTS)
import System.IO.Unsafe (unsafePerformIO)
import Data.Functor
import Data.List
import Control.Monad
import System.Random

-- 3rd library used named https://haskell-graphite.readthedocs.io/en/latest/
-- ## Create generators without QuickCheck (but basically the code from Exercise 2) ## --

-- A separate generator for the state, just numbers. Could have a bigger range
ltsGenState :: IO State
ltsGenState = randomRIO (0, 5)

-- A separate generator for the labels, just chars. Could go further, or generate random chars
ltsGenLabel :: IO Label
ltsGenLabel = do
    let labels = ['a'..'e']
    randomIndex <- randomRIO (0, length labels - 1)
    return [labels !! randomIndex]

-- A separate generator for the labeled transitions
ltsGenLabeledTransition :: [State] -> [Label] -> IO LabeledTransition
ltsGenLabeledTransition possibleStates possibleLabels = do
    firstState <- randomState possibleStates
    transitionLabel <- randomLabel possibleLabels
    lastState <- randomState possibleStates
    return (firstState, transitionLabel, if transitionLabel == delta then firstState else lastState)

-- Randomly select a state from a list of possible states
randomState :: [State] -> IO State
randomState possibleStates = do
    randomIndex <- randomRIO (0, length possibleStates - 1)
    return (possibleStates !! randomIndex)

-- Randomly select a label from a list of possible labels
randomLabel :: [Label] -> IO Label
randomLabel possibleLabels = do
    randomIndex <- randomRIO (0, length possibleLabels - 1)
    return (possibleLabels !! randomIndex)

-- Generate an IOLTS without delta states
ltsGen :: IO IOLTS
ltsGen = do
    setOfPossibleStates <- replicateM 5 ltsGenState Data.Functor.<&> nub
    initialStateIndex <- randomRIO (0, length setOfPossibleStates - 1)
    let initialState = setOfPossibleStates !! initialStateIndex
    setOfPossibleInputs <- replicateM 5 ltsGenLabel Data.Functor.<&> nub
    setOfPossibleOutputs <- replicateM 5 ltsGenLabel Data.Functor.<&> nub
    setOfPossibleLabeledTransitions <- replicateM 5 (ltsGenLabeledTransition setOfPossibleStates setOfPossibleInputs) Data.Functor.<&> nub
    return (nub setOfPossibleStates, setOfPossibleInputs, setOfPossibleOutputs, setOfPossibleLabeledTransitions, initialState)


-- Really not beautiful, but running out of time 
normalIolts :: IO IOLTS -> IOLTS
normalIolts = unsafePerformIO

visualizeLTS' :: [LabeledTransition] -> DGraph Integer Label
visualizeLTS' transitions = insertEdgeTriples (map (\(pre, trans, post) -> (pre,post,trans)) transitions) empty

visualizeLTS :: IOLTS -> DGraph Integer Label
visualizeLTS (_,_,_,transitions,_) = visualizeLTS' transitions

-- Visualize and store the buggy png in a temp folder
main =
    do
    randomLTS <- ltsGen
    plotDGraphPng (visualizeLTS (normalIolts ltsGen)) "./tmp/visualizedIOLTS"