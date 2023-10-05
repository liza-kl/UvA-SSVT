module Denis.Exercise2 where
import Denis.Exercise1
import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable
import System.Random
import System.IO.Unsafe

-- TODO THINK ABOUT HOW WE COULD USE PROPERTY... THE MUTATE FKT USES BOOL... CAN WE CONVERT?
countSurvivors:: Integer -> [([Integer] -> Gen [Integer])] -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Integer
countSurvivors amount [] properties func = 0
countSurvivors amount (x:xs) properties func = (getSurvivorsFromMutation amount x properties func) + (countSurvivors amount xs properties func)

-- That last mutate' param can be replaced by a random number... MAYBE USE THAT FOR GENERATE FUNCTION AS WELL...
getSurvivorsFromMutation:: Integer -> ([Integer] -> Gen [Integer]) -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Integer
getSurvivorsFromMutation 0 mutator properties func = 0
getSurvivorsFromMutation amount mutator properties func = do
        let mutantSurvived = generate (mutate' mutator properties func getRandomNumber) 
        if (unsafePerformIO(checkAllTrue mutantSurvived))
            then 1 + getSurvivorsFromMutation (amount - 1) mutator properties func 
            else 0 + getSurvivorsFromMutation (amount - 1) mutator properties func

checkAllTrue :: IO [Bool] -> IO Bool
checkAllTrue ioBools = do
    bools <- ioBools
    return (all (== True) bools)

getRandomNumber:: Integer
getRandomNumber = unsafePerformIO(generate (choose(1,1000)))

-- Function that returns a list of properties
properties :: [[Integer] -> Integer -> Bool]
properties = [prop_firstElementIsInput, prop_tenElements, prop_sumIsTriangleNumberTimesInput, prop_linear]

csTest :: Integer
csTest = countSurvivors 4000 [(addOneElement)] properties multiplicationTable