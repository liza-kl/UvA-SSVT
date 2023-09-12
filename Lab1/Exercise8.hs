{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
data Boy = Matthew | Peter | Jack | Arnold | Carl
            deriving (Eq,Show)

-- 3 tell the truth, 2 are lying
-- Matthew: Carl didn't do it, and neither did I. 
-- Peter: It was Matthew or it was Jack.
-- Jack: Matthew and Peter are both lying.
-- Arnold: Matthew or Peter is speaking the truth, but not both.
-- Carl: What Arnold says is not true

boys = [Matthew, Peter, Jack, Arnold, Carl]
accuses :: Boy -> Boy -> Bool -- Does the input boy accuse the 2nd boy 
accusers :: Boy -> [Boy] -- This boy is accused by XY 
guilty, honest :: [Boy]

-- accuses, determine if a boy is accusing another boy 
accuses Matthew otherBoy
    | otherBoy == Carl = False
    | otherBoy == Matthew = False
    | otherwise = True

accuses Peter otherBoy
    | otherBoy == Matthew = True
    | otherBoy == Jack = True
    | otherwise = False

-- Jack says that the accusements of Matthew and Peter are wrong
accuses Jack otherBoy
    | accuses Matthew otherBoy = False
    | accuses Peter otherBoy = False
    | otherwise = True

-- Arnold is accusing either the same persons as Matthew or Peter is accusing
accuses Arnold someBoy = (accuses Matthew someBoy || accuses Peter someBoy)
                        && not (accuses Matthew someBoy) && accuses Peter someBoy

-- Carl does not accuse the boys Arnold is accusing
accuses Carl otherBoy = not (accuses Arnold otherBoy)

-- Take every boy out of the list and check if they are accusing the input boy 
accusers accusedBoy = [ accuserBoy | accuserBoy <- boys, accuses accuserBoy accusedBoy ]

-- We have the information from the headmistress, that 3 boys always lie. 
guilty = [ guiltyBoy | guiltyBoy <- boys, 3 == length (accusers guiltyBoy)]

honest = [ honestBoy | honestBoy <- boys, accuses honestBoy (head guilty)]