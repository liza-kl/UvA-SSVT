module Denis.Exercise4 where
import Denis.Exercise2
import Denis.Exercise1
import MultiplicationTable
import Data.List
import Test.QuickCheck

calculateSurvivorPercentage:: Integer -> [([Integer] -> Gen [Integer])] -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Float
calculateSurvivorPercentage mutantAmount mutants properties func = fromIntegral (mutantAmount - (countSurvivors mutantAmount mutants properties func)) / fromIntegral mutantAmount

