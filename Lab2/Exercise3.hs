module Exercise3 where

import Exercise2
import Data.List
import Test.QuickCheck
import LTS

{-- TODO THIS CODE IS NOT WORKING! FEEL FREE TO DELETE --}
{-- 
Haskell program, QuickCheck Tests, random traces generator, indication of
time spent.
--}

-- Thought: Took the function from LTS module and modified it a bit.
iotraces :: IOLTS -> [[Label]]
iotraces (q, li, lo, lt, q0) = nub $ map snd (traces' lt [([q0],[])])

{-- This function checks for suspension traces in an IOLTS. 
According to Tretman, a suspension trace is one which includes a suspension label (in this case delta)
--}

{-- Maybe after first encountering delta, stop the trace generation at this point and go to the next trace --}

-- You can't filter a possible infinite list.
straces :: IOLTS -> [Trace]
straces iolts = nub (filter (\x -> delta `elem` x) (iotraces iolts))

-- Constaining suspension traces, as otherwise we could run into a problem where the list goes on for forever.
-- So no testing would be possible (due to the infinite list)
-- First thought: Checking for tau might not be that useful:
-- Second thought: A delta can be followed with a delta forever – stop the generation of the trace after 2 loops
-- 2 loops – to show that it is possible to loop. 

-- Function to check if an element is followed by the same element in a list
hasThreeConsecutiveDeltas :: Trace -> Bool
hasThreeConsecutiveDeltas [] = False  -- An empty list cannot have this property
hasThreeConsecutiveDeltas xs
    | length xs < 3 = False  -- The list must have at least three elements for consecutive "delta"s
    | otherwise = checkConsecutiveDeltas xs
  where
    checkConsecutiveDeltas (x:y:z:rest)
        | x == delta && y == delta && z == delta = True
        | otherwise = checkConsecutiveDeltas (y:z:rest)
    checkConsecutiveDeltas _ = False

-- genTrace :: Gen [Trace]
-- genTrace = do
--     ioltsToGen <- ltsGenDelta -- Get all transition tuples from a possible suspension IOLTS
--     elements ( (straces ioltsToGen) `suchThat` (not (hasThreeConsecutiveDeltas)))

-- According to Definition 9 in Tretmann
-- Introducing a prop with after would be nice which would check the state after delta.

prop_ioltsIncludesDelta :: IOLTS -> Bool 
prop_ioltsIncludesDelta (_,_,outputs,_,_) = delta `elem` outputs 
prop_allTracesIncludeDelta :: [[Label]] -> Bool
prop_allTracesIncludeDelta = all (elem delta)

