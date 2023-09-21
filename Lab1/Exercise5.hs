-- Time Spent: 90 Minutes

module Exercise5 where

data Boy = Matthew | Peter | Jack | Arnold | Carl
            deriving (Eq,Show)


-- 3 tell the truth, 2 are lying
-- Matthew: Carl didn't do it, and neither did I. He's accusing Peter, Jack, Arnold
-- Peter: It was Matthew or it was Jack.  He's accusing Matthew, Jack
-- Jack: Matthew and Peter are both lying. Negation of the accusements of Peter and Matthew
-- Arnold: Matthew or Peter is speaking the truth, but not both. Accusing the same people as Matthew or Peter, 
-- but Matthew accuses Arnold, so Arnold is probably supporting Peter (?)
-- Carl: What Arnold says is not true. Negation of Arnolds accusements

boys = [Matthew, Peter, Jack, Arnold, Carl] 
accuses :: Boy -> Boy -> Bool -- Does the input boy accuse the 2nd boy 
-- accuses, determine if a boy is accusing another boy 

{-- Just converting the text statements into code,
wildcard is used for the not mentioned cases in text--}
accuses Matthew Carl = False
accuses Matthew Matthew = False
accuses Matthew _ = True

{-- same here --}
accuses Peter Matthew = True
accuses Peter Jack = True
accuses Peter _ = False

-- Jack says that the accusements of Matthew and Peter are wrong,
-- so he is not accusing the boys Peter and Matthew are accusing and vice versa.
accuses Jack otherBoy 
    | accuses Matthew otherBoy = False -- if Matthew accuses another boy, Jack is not accusing him
    | accuses Peter otherBoy = False -- if Peter accuses another boy, Jack is not accusing him
    | otherwise = True

-- Arnold is accusing either the same persons as Matthew or Peter is accusing
-- but Matthew accuses Arnold, so Arnold is probably supporting Peter (?)
accuses Arnold someBoy = (accuses Matthew someBoy || accuses Peter someBoy)
                        && not (accuses Matthew someBoy) && accuses Peter someBoy

-- Carl does not accuse the boys Arnold is accusing
accuses Carl otherBoy = not (accuses Arnold otherBoy)

-- Take every boy out of the list and check if they are accusing the input boy 
accusers :: Boy -> [Boy] -- This boy is accused by XY 
accusers accusedBoy = [ accuserBoy | accuserBoy <- boys, accuses accuserBoy accusedBoy ]

-- 3 boys are always telling the truth, so find a boy which is accused by 3 others

guilty, honest :: [Boy]
guilty = [ guiltyBoy | guiltyBoy <- boys, 3 == length (accusers guiltyBoy)]

-- Guilty should be a list of only one boy but for the function signature you need a Boy, so take head.
honest = [ honestBoy | honestBoy <- boys, accuses honestBoy (head guilty)]