module Denis.Exercise1 where
import Test.QuickCheck
import Data.List
import System.Random
import SetOrd

-- Time estimate 30 mins. (Thanks syntax errors... 🙃)

randomInt:: Gen Int
randomInt = arbitrary

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

main :: IO ()
main = do
    value <- generate generateSets
    print value