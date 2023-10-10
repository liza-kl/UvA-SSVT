module Exercise1 where
import Test.QuickCheck
import Data.List
import System.Random
import SetOrd

-- generate an integer given a min and max value
randomInt :: Int -> Int -> IO Int
randomInt min max = randomRIO (min, max)

-- generate a set of integers with a given size
-- if size = 0, return empty set
-- otherwise, generate random integers "size" times 
-- and insert them into the set
-- do it recursively
generateSet :: Int -> Int -> Int -> IO (Set Int)
generateSet 0 _ _ = return emptySet
generateSet size min max = do
    randInt <- randomInt min max
    rest <- generateSet (size - 1) min max
    return (insertSet randInt rest)

main :: IO ()
main = do
    let size = 10 -- max set size
    let min = 1 -- integer min value
    let max = 100 -- integer max value

    randomSet <- generateSet size min max
    putStrLn $ "Random Set: " ++ show randomSet
