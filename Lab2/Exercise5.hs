module Exercise5 where
import Data.List
import Data.Maybe
import Exercise4 
import LTS
import Debug.Trace (trace)
import Control.Exception (catch, SomeException)

-- ## Deliverables ##
-- Haskell program, description of each bug, indication of time spent.
-- Time spent: 180 min without success

-- ## List of Bugs ##

    -- - doorImpl1: Correct by definition
    -- - doorImpl2: when it closes, it goes to an opened state
    -- - doorImpl3: when it unlocks, it transitions to the wrong state (2)
    -- - doorImpl4: When you are in State (1) and "unlock" the door, it's locked and in state (2), when you are in state (2) and lock the door, its unlocked and in state (1) 
    -- - doorImpl5: Runs infinitely, but the code is the same as the door implementation 1. Therefore it should be ioco true
    -- - doorImpl6: After the trace ["closed","open"], the door is { locked }, but the model is {closed }, and it says the door is stuck
    -- - doorImpl7: Runs infinitely, but if looking at the implementation, it says something like "wrong keycode". The implementation is different though but we can't confirm or deny ioco since it gets stuck in an infinite loop. Probably not ioco though.
    -- - doorImpl8: After the trace ["closed"], doorImpl8 returns { closed }, but the model is {locked, opened} 

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

{-- The correctness of the implementation can be tests with ioco against the other door implementations --}

-- We added the pre-existing door implementations manually as an IOLTS.
{-- 
    We couldn't figure out how to generate these IOLTS dynamically
    since that would require analysing the pattern-matched parameters
    as well as their results of each implementation.
    If we were to generate this, we would take those and generate
    those tuples based on the corresponding parameters of the given implementations.
    
    This function receives the model and the implementation
    First of all, another function should be created that will convert the implementation to an IOLTS
        - will call the implementation function with 0 and any of these labels = ["open", "close", "lock", "unlock"]
        - and the repeat for states we get
        - then (state, label) tuples can be created and added to a list
        - catch any exceptions occured
        - call createIOLTS with this list
--}

-- Door Implementation 1 (Pre-defined as the correct one...)
correctDoorModel :: IOLTS
correctDoorModel = createIOLTS [
                (0,"?close", 1),(0,"!closed",1),
                (1,"?open", 0),(1,"!opened",0),
                (1, "?lock", 2), (1,"!locked",2),
                (2, "?unlock", 1), (2,"!unlocked",1)]

doorImpl2ToIOLTS :: IOLTS
doorImpl2ToIOLTS = createIOLTS [
                (0,"?close", 1),(0,"!opened",1),
                (1,"?open", 0),(1,"!closed",0),
                (1, "?lock", 2), (1,"!locked",2),
                (2, "?unlock", 1), (2,"!unlocked",1)]

doorImpl3ToIOLTS :: IOLTS
doorImpl3ToIOLTS = createIOLTS [
                (0,"?close", 1),(0,"!closed",1),
                (1,"?open", 0),(1,"!opened",0),
                (1, "?lock", 2), (1,"!locked",2),
                (2, "?unlock", 2), (2,"!unlocked",2)]

doorImpl4ToIOLTS :: IOLTS
doorImpl4ToIOLTS = createIOLTS [
                (0,"?close",1), (0, "!closed", 1),
                (1, "?open", 0), (1,"!opened",0),
                (1,"?unlock",2),(1,"!locked",2),
                (2,"?lock",1),(2,"!unlocked",1)]

doorImpl5ToIOLTS :: IOLTS
doorImpl5ToIOLTS = createIOLTS[
                    (0,"?close", 1),(0,"!closed",1),
                    (1,"?open", 0),(1,"!opened",0),
                    (1, "?lock", 2), (1,"!locked",2),
                    (2, "?unlock", 1), (2,"!unlocked",1)]

doorImpl6ToIOLTS :: IOLTS 
doorImpl6ToIOLTS = createIOLTS[
    (0, "?close", 1),(0,"!closed",1),
    (1, "?open", 3),(1,"!opened",3),
    (3, "?close", 4),(3,"!closed",4),
    (4, "?open", 5),(4,"!opened",5),
    (5, "?close", 6),(5,"!closed",6),
    (6, "?open", 6),(6,"delta",6),
    (1, "?lock", 2),(1,"!locked",2),
    (4, "?lock", 2),(1,"!locked",2),
    (6, "?lock", 2),(6,"!locked",2),
    (2, "?unlock", 1),(2,"!unlocked",1)]

doorImpl7ToIOLTS :: IOLTS
doorImpl7ToIOLTS = createIOLTS [
    (0,"?close", 1), (0,"!closed",1),
    (1,"?open", 0), (1,"!opened",0),
    (1, "?lock", 2), (1,"!locked",2),
    (2, "?unlock", 3), (2,"!unlocked",3),
    (4, "?close", 5), (4,"!closed",5),
    (3, "?open", 4), (3,"!opened",4),
    (3, "?lock", 2), (3,"!locked",2),
	(5, "?open", 0), (5,"!opened",0),
	(5, "?lock", 6), (5,"!locked",6)]

doorImpl8ToIOLTS :: IOLTS
doorImpl8ToIOLTS = createIOLTS [
    (0,"?close", 1),(0,"!closed",1),
    (1,"?open", 0),(1,"!opened",0),
    (1, "?lock", 2), (1,"!locked",2),
    (2, "?unlock", 3), (2,"!unlocked",3),
    (4, "?close", 5), (4,"!closed",5),
    (3, "?open", 4), (3,"!opened",4),
    (3, "?lock", 2), (3,"!locked",2),
    (5, "?open", 6), (5,"!opened",6),
    (5, "?lock", 2), (5,"!locked",2),
    (6, "?close", 7), (6,"!closed",7),
    (7, "?lock", 2), (7,"!locked",2),
    (7, "?close", 2), (7,"!closed",2)]

{-- 
    Retrieves a list of all the given pre-defined IOLTS which we hardcoded above.
    Right now we include not all of them. If two are equal then the LTS against SUT check
    would run indefinetely. 
    The infinite loops are probably happening due to open-close loops inside of the IOLTS.
    This concerns door implementation 5 and 7, which is why we left them out of this list,
    to have a running test.
--}
getDoorLTSs :: [IOLTS]
getDoorLTSs = [doorImpl2ToIOLTS, doorImpl3ToIOLTS, doorImpl4ToIOLTS, doorImpl6ToIOLTS, doorImpl8ToIOLTS]

-- Check of a single IOLTS implementation (like the hard-coded ones above...) if it lines up with the correct model.
-- Just runs "ioco" for the model and the implementation.
testLTSAgainstSUT :: IOLTS -> (State -> Label -> (State, Label)) -> Bool
testLTSAgainstSUT model impl = 
    trace ("ioco: " ++ show (ioco correctDoorModel model)) $
    ioco correctDoorModel model

-- Helper function to check multiple IOLTS models at once against the correct model.
-- Just for easier final test execution and better overview.
testLTSsAgainstSUT :: [IOLTS] -> (State -> Label -> (State, Label)) -> [(String,Bool)]
testLTSsAgainstSUT models impl = map (\impl -> ("Door " ++ show ( ((fromMaybe 0 (elemIndex impl models)) + 2) ),impl `ioco` correctDoorModel)) getDoorLTSs

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
    print (testLTSsAgainstSUT getDoorLTSs doorImpl1)