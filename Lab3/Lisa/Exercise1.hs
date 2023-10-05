module Lisa.Exercise1 where

    import Data.List
    import Test.QuickCheck
    import Mutation


    mutatorEmptyList :: [Integer] -> Gen [Integer]
    mutatorEmptyList _ = do return []

    mutator_permutate

-- ## Deliverables ##
-- List of mutators and rationale, implementation, indication of time spent.
-- Time spent: 

-- ## What is not covered? ##

-- Empty lists; Return an empty list – example taken from Stryker 
-- https://stryker-mutator.io/docs/mutation-testing-elements/supported-mutators/#array-declaration
-- Emptying the function body (or in Haskell return undefined)
-- Inspired from https://stryker-mutator.io/docs/mutation-testing-elements/supported-mutators/#block-statement
-- Reversed Lists 
-- Shuffled Lists 
-- While Loop 
-- Runtime Errors 
-- All items are unique 

-- ## Reasoning of strength ##

-- For this we can use Hoare's theory, Weak to Strong
-- Ran the function in the Mutation.hs file and got the following 

-- 0 survivors (100% killed).
-- apparent minimal property subsets:  {1}
-- conjectures:  {3}   ==> {5}     96% killed (weak)
--              {2,4} ==> {5}     99% killed (weak)
--              {3,4} ==> {2}     99% killed (weak)

-- According to the report, {1} is equal to {1,2,3,4,5}. This depends on the
-- "[...] number of killed mutants, each of these subsets is as strong as [all properties together]" – derived from 
-- FitSpec: Refining Property Sets for Functional Testing paper Example #1. 

-- We can assume that 1 is the strongest
-- Followed by 5, because {2,4} and {3} are subsets of five
-- Then 2, because 2 has the subset of {3,4}
-- Then 4, as this only applies to lists with "multipliers"
-- Then 3, as it covers more possibilities.


-- ## List of possible other mutators ##
-- Empty List 

