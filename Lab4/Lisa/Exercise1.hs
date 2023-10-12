module Exercise1 where

import SetOrd
import System.Random
import Control.Monad
randomInt :: IO Int
randomInt = do
    randomRIO (1,100) :: IO Int


-- Sort the min and max value in correct order 
getTupleAndSort :: IO Int -> IO Int -> IO (Int, Int)
getTupleAndSort ioNum1 ioNum2 = do
    num1 <- ioNum1
    num2 <- ioNum2
    if num1 == num2 || num1 < num2
        then return (num1, num2)
        else return (num2, num1)


-- Generator to create a list of random numbers 
randomIntList :: IO Int -> (IO Int, IO Int) -> [IO Int]
randomIntList nRange tupleRange = do
    n <- nRange
    (minValue, maxValue) <- tupleRange
    replicateM n (randomRIO (minValue, maxValue))


randomSetGen :: IO (Set Int)
randomSetGen = do
    randomNum <- randomInt
    num1 <- randomInt
    num2 <- randomInt
    return (list2set [(randomIntList randomNum num1 num2)])

-- QuickCheck Generator 