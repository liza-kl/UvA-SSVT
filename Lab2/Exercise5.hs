module Exercise5 where

import LTS
{-- Haskell program, description of each bug, indication of time spent. --}

-- Taking the door 1 implementation as a basis for the IOLTS 
-- We need to implement the output function in order to check other stuff
-- Definition found in presentation slides 
out :: IOLTS -> [State] -> [Label]
-- case for when the state list is empty, where we return the delta LTS datatype
out  (states,inputs,outputs,transitions,state) [] = [delta]
out (_, _, _, transitions, _) states =
    map (\s -> nextTransitions' transitions s) states




-- 


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

-- testLTSAgainstSUT :: IOLTS -> (State -> Label -> (State, Label)) -> Bool

-- In the end you need to print smt like doorImplX ioco correctDoorModel
main :: IO ()
main =
    print "a door"