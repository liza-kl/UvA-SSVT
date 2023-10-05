module Denis.Exercise1 where
import Data.List
import Test.QuickCheck
import Mutation


-- We wanna handle other edge cases for mutating lists.


randomList:: [Integer] -> Gen [Integer]
randomList givenList = arbitrary::Gen [Integer]

emptyList:: [Integer] -> Gen [Integer]
emptyList givenList = (arbitrary :: Gen [Integer]) `suchThat` (== [])

shuffleList:: [Integer] -> Gen [Integer]
shuffleList givenList = shuffle givenList

addOneElement:: [Integer] -> Gen [Integer]
addOneElement givenList = do
                            return (1 : givenList)

-- TODO MORE