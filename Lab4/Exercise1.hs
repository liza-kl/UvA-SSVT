module Exercise1 where
import Test.QuickCheck
import Data.List
import System.Random
import SetOrd

-- Time estimate 30 mins. (Thanks syntax errors... 🙃)

randomInt:: Gen Int
randomInt = arbitrary

-- 16807 also possible
randomLCGInt :: Gen Int
randomLCGInt = do
    -- (seed * multiplier) % modulus 
    -- Alternative lel
    -- currentTime <- getCurrentTime
    -- let seed = round (utctDayTime currentTime)
    let nextSeed = (1256785 * 48271) `rem` 2147483647
    return nextSeed

generateRandomList:: Int -> Gen [Int]
generateRandomList 0 = pure []
generateRandomList size = do
            randInt <- randomInt
            rest <- generateRandomList (size - 1)
            return (randInt : rest)

generateRandomSet:: Int -> Gen (Set Int)
generateRandomSet size = do
                randomList <- generateRandomList size
                return (list2set randomList)

generateSets :: Gen (Set Int)
generateSets = do
            randInt <- randomInt
            generateRandomSet randInt

generateRandomList':: Gen [Int]
generateRandomList' = arbitrary

generateSets':: Gen (Set Int)
generateSets' = do
                list2set <$> generateRandomList'

main :: IO ()
main = do
    value1 <- generate generateSets
    value2 <- generate generateSets'
    print value1
    print value2