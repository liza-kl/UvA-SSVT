-- Import the necessary modules
import Test.FitSpec

-- Define the corona1 function
corona1 :: Int -> Int -> Int -> Int -> Int
corona1 r s x0 t = last.init $ scanl (\a b -> s + r * a) x0 [0..t]

-- Define a property to test the corona1 function
-- This property checks that the function satisfies a certain condition.
prop_corona1 :: Int -> Int -> Int -> Int -> Bool
prop_corona1 r s x0 t =
    corona1 r s x0 t == last.init (scanl (\a b -> s + r * a) x0 [0..t])

-- Configure fitspec using args
myArgs = args
    { names = ["corona1"]
    , nMutants = 10
    , nTests = 10
    , timeout = 5
    }

-- Run mutation testing with fitspec
testFitSpec = reportWith myArgs
    (corona1 :: Int -> Int -> Int -> Int -> Int)
    [property prop_corona1]  -- Pass the property without specific arguments
