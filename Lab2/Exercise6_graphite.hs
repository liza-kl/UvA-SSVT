

module Exercise6Graphite where
import GHC.Generics (Generic)
import Data.Hashable

import Data.Graph.DGraph
import Data.Graph.UGraph
import Data.Graph.Visualize
import Data.Graph.Types
import LTS
import Exercise5 (doorImpl5ToIOLTS)


visualizeLTS' :: [LabeledTransition] -> DGraph Integer String
visualizeLTS' transitions = fromArcsList (map (\(pre, trans, post) -> Arc pre post trans) transitions)

visualizeLTS (_,_,_,transitions,_)= visualizeLTS' transitions


main = 
    plotDGraphPng (visualizeLTS tretmanR2) "./tmp/tretmanR2"