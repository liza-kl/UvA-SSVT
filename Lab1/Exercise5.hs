module Exercise5 where

data Boy = Matthew | Peter | Jack | Arnold | Carl
            deriving (Eq,Show)

-- 3 tell the truth, 2 are lying
-- Matthew: Carl didn't do it, and neither did I. This leaves us with Peter, Jack, Arnold
-- Peter: It was Matthew or it was Jack.  Accused Matthew, Jack
-- Jack: Matthew and Peter are both lying.
-- Arnold: Matthew or Peter is speaking the truth, but not both. Arnold supports Peters version
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

-- accuses Peter Matthew = True <- write is this way to make it cleaner
--     -- | otherBoy ==  = True
--     -- | otherBoy == Jack = True
--     -- | otherwise = False
accuses Peter otherBoy 
    | otherBoy ==  Matthew = True
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

-- 3 boys are always telling the truth, so find a boy which is accused by 3 others
guilty = [ guiltyBoy | guiltyBoy <- boys, 3 == length (accusers guiltyBoy)]

-- Guilty should be a list of only one boy but for the function signature you need a Boy, so take head.
honest = [ honestBoy | honestBoy <- boys, accuses honestBoy (head guilty)]