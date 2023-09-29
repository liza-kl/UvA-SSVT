module Exercise3 where

import Exercise2
import Data.List
import Test.QuickCheck
import LTS

-- ## Deliverables ##
--  Haskell program, QuickCheck Tests, random traces generator, indication of
-- time spent: ~ 5 hours without any success

-- ## Thoughts on this ##
-- The first naive approach was to generate traces from an IOLTS and then "just" filter for the delta label.
-- But this would end in an infinite list, so no filtering is actually possible.
-- since suspension traces can lead to loops (so you'll get something like delta,delta,delta...).
-- According to Tretman, a suspension trace is one which includes a suspension label (in this case delta)
-- Maybe after first encountering delta, stop the trace generation at this point and go to the next trace -- but this is impossible (see hasThreeConsecutiveDeltas)
-- So in order to get this traces, we probably should have searched for traces _before_ the quiescence state 
-- (which leads to this looping).
-- We could possible achieve that with a breadth first seach and then traversing the tree, looking for the possible
-- traces. 
-- Furthermore, tau states need to be considered to "merge" the states together


-- ## Non working code ##

-- iotraces :: IOLTS -> [[Label]]
-- iotraces (q, li, lo, lt, q0) = nub $ map snd (traces' lt [([q0],[])])

-- straces :: IOLTS -> [Trace]
-- straces iolts = nub (filter (\x -> delta `elem` x) (iotraces iolts)) -- not possible

-- Function to check if an element is followed by the same element in a list
-- hasThreeConsecutiveDeltas :: Trace -> Bool
-- hasThreeConsecutiveDeltas [] = False  -- An empty list cannot have this property
-- hasThreeConsecutiveDeltas xs
--     | length xs < 3 = False  -- The list must have at least three elements for consecutive "delta"s
--     | otherwise = checkConsecutiveDeltas xs
--   where
--     checkConsecutiveDeltas (x:y:z:rest)
--         | x == delta && y == delta && z == delta = True
--         | otherwise = checkConsecutiveDeltas (y:z:rest)
--     checkConsecutiveDeltas _ = False

-- The started traces generator 
-- genTrace :: Gen [Trace]
-- genTrace = do
--     ioltsToGen <- ltsGenDelta -- Get all transition tuples from a possible suspension IOLTS
--     elements ( (straces ioltsToGen) `suchThat` (not (hasThreeConsecutiveDeltas)))


-- According to Definition 9 in Tretmann
-- Introducing a prop with after would be nice which would check the state after delta.
-- prop_ioltsIncludesDelta :: IOLTS -> Bool 
-- prop_ioltsIncludesDelta (_,_,outputs,_,_) = delta `elem` outputs 
-- prop_allTracesIncludeDelta :: [[Label]] -> Bool
-- prop_allTracesIncludeDelta = all (elem delta)

