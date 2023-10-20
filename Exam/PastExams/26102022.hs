module Exam26102022 where
import Test.FitSpec
import LTS
import Test.QuickCheck
import System.Random
import Control.Monad (replicateM)
import AfterImpl
import OutImpl
import Debug.Trace

-- corona :: Int -> Int -> Int -> Int -> Int
-- corona r s x0 t = iterate ((s+).(r*)) x0 !! t
-- corona2 r s x0 t = (r^t-1) `div` (r-1)*s + r^t*x0
-- corona3 r s x0 t = foldr (-) x0 [s+r^d*x0 | d <- [0..t]]
-- corona4 r s x0 t = (!! t) $ zipWith (+) (iterate (+s) 0) (iterate (*r) x0)
-- corona5 r s x0 t = (!! max 0 t) $ iterate (\c -> s+r*c) x0

-- prop_allInputsGT0 :: (Int -> Int -> Int -> Int -> Int) -> Int -> Int -> Int -> Int -> Bool
-- prop_allInputsGT0 func r s x0 t = all (>= 0) [r,s,x0,t]
-- prop_ZeroOutputsZero :: (Int -> Int -> Int -> Int -> Int) -> Int -> Int -> Int -> Int -> Bool
-- prop_ZeroOutputsZero func r s x0 t  = func r s x0 0 == 0
-- prop_SameOutputAsEquation ::  (Int -> Int -> Int -> Int -> Int) -> Int -> Int -> Int -> Int -> Bool
-- prop_SameOutputAsEquation func r s x0 t =  corona r s x0 (t+1) - corona r s x0 t  == (r - 1) * corona r s x0 t + s


-- properties corona=
--     [
--     property $ prop_allInputsGT0 corona,
--     property $ prop_ZeroOutputsZero corona,
--     property $ prop_SameOutputAsEquation corona
--     ]

-- testFitSpec = mainWith args { names = ["corona r s x0 t"]
--                     , nMutants = 4000
--                     , nTests = 4000
--                     , timeout = 0
--                     }
--                 (corona :: Int -> Int -> Int -> Int -> Int)
--                 properties


-- LTS Stuff 

coffeeImpl :: LTS
coffeeImpl = ([1..4], ["?btn", "?btn_milk", "!espresso", "!latte"], [(1, "?btn", 2), (1, "?btn_milk", 1), (2, "!espresso", 3), (2, "!latte", 4)], 1)

coffeeModel :: LTS
coffeeModel = ([1..5], ["?btn", "?btn_milk", "!espresso", "!latte"],[(1, "?btn", 2), (2, "!espresso", 3), (1, "?btn_milk", 4), (4,"!latte", 5)], 1)


seed::Int
seed = 40
generator = mkStdGen seed

getRandomElement :: [a] -> IO a
getRandomElement xs = do
    index <- randomRIO (0, length xs - 1)
    return (xs !! index)

ourContains:: Eq a => [a] -> a -> Bool
ourContains [] _ = False
ourContains (x:xs) elemToCheck
    | x == elemToCheck = True
    | otherwise = ourContains xs elemToCheck


-- Will coffee be dispense or not. Basically you are looking for the out function.
-- type IOLTS = ([State], [Label], [Label], [LabeledTransition], State)

traceResult :: Trace -> [Label]
traceResult traceToCheck =
             let coffeeImplStuff = [(1, "?btn", 2), (2, "!espresso", 3), (1, "?btn_milk", 4), (4,"!latte", 5)]
                 coffeeIOLTS = createIOLTS coffeeImplStuff
                 result = out' coffeeIOLTS (coffeeIOLTS `after'` traceToCheck)
            in result

quantumCoffee :: Trace -> Bool
quantumCoffee traceToCheck =
        let coffeeImplStuff = [(1, "?btn", 2), (2, "!espresso", 3), (1, "?btn_milk", 4), (4,"!latte", 5)]
            coffeeIOLTS = createIOLTS coffeeImplStuff
            result = out' coffeeIOLTS (coffeeIOLTS `after'` traceToCheck)
    in
        ourContains result "latte"

main = do
    -- Define or import traces and coffeeModel
    let myList = traces coffeeModel
    -- You need to provide the 'trace' argument when calling quantumCoffee
    let traceToCheck =  ["?btn_milk"]-- Example trace
    let result = (if quantumCoffee traceToCheck then "Latte dispensed" else "Latte not dispensed")
    print  result