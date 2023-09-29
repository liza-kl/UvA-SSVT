module Exercise5 where
import Data.List
import Exercise4 
import LTS
import Debug.Trace (trace)
import Control.Exception (catch, SomeException)

{-- Haskell program, description of each bug, indication of time spent. --}

-- Taking the door 1 implementation as a basis for the IOLTS 
-- We need to implement the output function in order to check other stuff
-- Definition found in presentation slides 

out :: IOLTS -> [State] -> [Label]
-- case for when the state list is empty, where we return the delta LTS datatype
out  (states, inputs, outputs, transitions, state) [] = [delta]
out (_, _, outputs, transitions, _) states =
    filter (`elem` outputs) (nub $ concatMap (map snd . nextTransitions' transitions) states)

ioltstraces :: IOLTS -> [Trace] -- [[Label]]
ioltstraces (q, i, o, lt, q0) = nub $ map snd (traces' lt [([q0],[])])

{-- 
doorImpl1 :: State -> Label -> (State, Label)
doorImpl1 0 "close" = (1, "closed")
doorImpl1 1 "open" = (0, "opened")
doorImpl1 1 "lock" = (2, "locked")
doorImpl1 2 "unlock" = (1, "unlocked")
doorImpl1 _ _ = error "Invalid command and/or state!"
--}
{-- The correctness of the implementation can be tests with ioco against the other door implementations --}

correctDoorModel :: IOLTS
correctDoorModel = createIOLTS [
                (0,"?close", 1),(0,"!closed",1),
                (1,"?open", 0),(1,"!opened",0),
                (1, "?lock", 2), (1,"!locked",2),
                (2, "?unlock", 1), (2,"!unlocked",1)]


wrongDoorModel :: IOLTS
wrongDoorModel = createIOLTS [
                (0,"?close", 1),(0,"!opened",1),
                (1,"?open", 0),(1,"!closed",0),
                (1, "?lock", 2), (1,"!locked",2),
                (2, "?unlock", 1), (2,"!unlocked",1)]


{-- This function receives the model and the implementation
    First of all, another function should be created that will convert the implementation to an IOLTS
        - will call the implementation function with 0 and any of these labels = ["open", "close", "lock", "unlock"]
        - and the repeat for states we get
        - then (state, label) tuples can be created and added to a list
        - catch any exceptions occured
        - call createIOLTS with this list
    Then, testLTSAgainstSUT will just call ioco with the two IOLTS's and return the result
--}

testLTSAgainstSUT :: IOLTS -> (State -> Label -> (State, Label)) -> Bool
testLTSAgainstSUT model impl = 
    trace ("ioco: " ++ show (ioco wrongDoorModel correctDoorModel)) $
    ioco wrongDoorModel correctDoorModel

-- Define ioco Function to get the outputs.
-- Definition of ioco is for all (s)traces of the model must confirm: out ( implementation after trace ) is a subset
-- of out (model after trace)
ioco :: IOLTS -> IOLTS -> Bool 
ioco implementation model = and
    [ trace ("Traces: " ++ show traces) $
      trace ("i: " ++ show i) $
      trace ("m: " ++ show (out model (model `after` traces))) $
      i `elem` (out model (model `after` traces))
    | traces <- ioltstraces model
    , i     <- out implementation (implementation `after` traces)
    ]

-- In the end you need to print smt like doorImplX ioco correctDoorModel
main :: IO ()
main = do
    print (testLTSAgainstSUT correctDoorModel doorImpl2)