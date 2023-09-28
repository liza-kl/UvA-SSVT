module Exercise5 where

import LTS 
{-- Haskell program, description of each bug, indication of time spent. --}

-- Taking the door 1 implementation as a basis for the IOLTS 

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
    do 
        print "a door"