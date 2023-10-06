module Exercise6 where

import Data.List (intercalate)
import Exercise2
import Mutation
import Exercise1
import Test.QuickCheck
import MultiplicationTable
import System.IO.Unsafe
import MultiplicationTable (prop_moduloIsZero)

-- ## Task ##
-- Create a function that we pass the function under test, 
-- a set of properties, and a number indicating the number of mutants,
-- that visualizes the results of the functions from the previous exercises.

-- ## Approach ##
-- 1.) Print it in a table in the terminal
-- 2.) Use a plotting library like https://github.com/timbod7/haskell-chart/wiki 
-- or https://hackage.haskell.org/package/QuickPlot


-- We want a report (maybe a table or graph) which shows the output of the previous functions.
-- Function should take as arguments a set of properties, a number of mutants
-- and the function under test.
-- To make it more readable create for every row that needs to be produced a tablerow
-- So we need one for countSurvivors (Exercise 2)
-- One for minimal subsetOfProperties (Exercise 3)
-- One Table for strength (Exercise 4)
-- One Table for created conjenctures (Exercise 5) 

-- ## Implementation ##

getMutators = [shuffleList]
getproperties = [prop_tenElements, prop_firstElementIsInput, prop_sumIsTriangleNumberTimesInput, prop_linear, prop_moduloIsZero]

representSurvivorsAndKilled :: IO Integer
representSurvivorsAndKilled = do
    survivors <- generate $ countSurvivors 4000 [prop_linear] shuffleList multiplicationTable
    return survivors



-- Define a data structure for a row in the table
data TableRow = TableRow { name :: String, result :: Integer }

-- Create a list of sample data
sampleData :: [TableRow]
sampleData =
    [ TableRow "Killed Mutants in Percentage" 35
    , TableRow "Used Mutants" ()
    , TableRow "Used Properties" ()
    , TableRow "Survivors" (unsafePerformIO representSurvivorsAndKilled)
    ]

-- Function to print the table
printTable :: [TableRow] -> IO ()
printTable rows = do
    putStrLn mainHeader
    putStrLn header
    putStrLn separator
    mapM_ printRow rows
    where
    mainHeader ="| REPORT                          |"
    header =    "| Function | Result  |            |"
    separator = "+----------+---------+------------+"

    printRow row = putStrLn $ "| " ++ formatField (name row) ++ " | "
                            ++ formatField (show $ result row) ++ " | "

    formatField field = let width = 20 in take width (field ++ repeat ' ')

main :: IO ()
main = do
    putStrLn "Table Example:"
    printTable sampleData
