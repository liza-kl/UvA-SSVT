module Exercise2 where
import Data.List
import Test.QuickCheck
import FitSpec
import Exercise1
import Mutation
import System.Random
import Control.Monad

{--
    countSurvivors calls mutate function for each property and mutator, 
    the function under test and a random input.
    and it returns how many times it returned "Mutant survived".
--}

-- number of mutants
-- list of properties
-- a mutant
-- function under test, multiplicationTable
countSurvivors
    :: Integer
    -> [a -> Integer -> Bool]
    -> ([Integer] -> Gen [Integer])
    -> (Integer -> [Integer])
    -> Integer
countSurvivors nMutants props mutant func = do
    let initialCount = 0
    survivors <- foldM (\count prop -> do
        result <- generate (mutate mutant prop multiplicationTable 5)
        if result == Just True
            then return (count + 1)
            else return count
        ) initialCount props
    return survivors


main :: IO ()
main =  do
    let props = [prop_tenElements, prop_firstElementIsInput, prop_sumIsTriangleNumberTimesInput, prop_linear, prop_moduloIsZero]
    let mutants = [("addElements", addElements), ("removeElements", removeElements), ("emptyList", emptyList), ("sortList", sortList), ("reverseList", reverseList), ("shuffleList", shuffleList), ("negativeList", negativeList), ("changeList", changeList), ("sameNumberList", sameNumberList), ("addElementsRandomly", addElementsRandomly)]
    mapM_ (\(mutantName, mutant) -> do
        let count = countSurvivors 4000 props mutant multiplicationTable
        putStrLn $ "Mutant: " ++ mutantName ++ ", Survivors: " ++ show count
        ) mutants