{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Exam27102021 where 
-- that checks whether each state is
-- reachable from the initial state
-- Exam 27.10.2021

-- Approach: Using the nextTransitions Functions and filter out the states from the
-- list which are not reachable from the initial state. 
-- We get the initial state from the definition
-- Or the after function 


-- hasUnreachableStates :: LTS -> Bool
-- hasUnreachableStates (setOfStates, labeledTransitions, initialState) = 
--     let allTraces = traces (setOfStates, labeledTransitions, initialState) 
--     in 
--         -- if (after function contains states that are not in allTraces which 
--         --start with the initial state, return false, else true.)

numK :: Int -> Int -> Int
numK _ 0 = 0
numK _ 1 = 1
numK n k = sum $ map (\x -> numK (n-x) (k-1)) [0..n]

prove_numK int1 int2 =
    numK int1 int2 == numK (int1+1) int2 