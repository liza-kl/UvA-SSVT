

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
-- ## Indication of time spent: 90 Minutes ##

-- ## Thoughts ## 
-- At first we wanted to print in the terminal somehow, but since our skills aren't that good
-- we decided to look into 3rd part libraries and think of IOLTS as somehow of directed graphs
-- We managed to use our custom lts generator, but the png is buggy and we didn't have an idea why.
-- So this is our buggy visualizer, but at least we learnt how to download and import 3rd party libraries in Haskell

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


-- Really not the way to go, but we hadn't got other ideas.
normalIolts :: IO IOLTS -> IOLTS
normalIolts = unsafePerformIO

-- Helper function to transfer the incoming labeled transitions to the required type
-- Inspiration was "someDirectedGraph" from https://haskell-graphite.readthedocs.io/en/latest/basic/
visualizeLTS' :: [LabeledTransition] -> DGraph Integer Label
visualizeLTS' transitions = insertEdgeTriples (map (\(pre, trans, post) -> (pre,post,trans)) transitions) empty

-- Actual function which takes an LTS 
visualizeLTS (_,_,_,transitions,_) =
    let adaptedData = visualizeLTS' transitions
    in plotDGraphPng adaptedData "./tmp/visualizedIOLTS" -- maybe it would be better to have a dynamic name

-- main function which calls it with our generator.
main =
    do
    randomLTS <- ltsGen
    visualizeLTS (normalIolts ltsGen)