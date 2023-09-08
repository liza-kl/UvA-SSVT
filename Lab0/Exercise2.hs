-- Exercise 2
-- Time spent ~2.5-3h due to difficulties with Haskell...

module Exercise2 where
import Data.List
import Data.Char
import System.IO.Unsafe
import System.Random
import Test.QuickCheck

-- Given Function to generate the random list of numbers
probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1)
    return (p:ps)

-- Workaround to get the randomized list of floats in a non-IO list.
genProbs:: Int -> [Float]
genProbs num = unsafePerformIO(probs num)

-- Does the filtering on each quartile. Quartiles go from 0 to 3, for the first, second ... quartile.
filterQuartile:: [Float] -> Float -> [Float]
filterQuartile probs quartile = filter (\x -> x >= 0.0 + quartile * 0.25 && x < 0.25 + quartile * 0.25) probs

-- Determines the amount of elements in a certain quartile given the complete list of probs.
filterQuartileAmount:: [Float] -> Float -> Int
filterQuartileAmount probs quartile = length (filterQuartile probs quartile) 

-- We factor in a ~5% deviation of our quartile amounts in this test.
isLengthInBound:: Int -> Int -> Bool
isLengthInBound amount originalAmount = ((originalAmount `quot` 42) * 10 ) < amount && (originalAmount `quot` 38) * 10  > amount

-- Determines if for a certain quartile roughly (with the 5% deviation) one quarter of the given elements exist and not too few/too many.
isQuartileInBound:: [Float] -> Float -> Bool
isQuartileInBound probs quartile = isLengthInBound (filterQuartileAmount probs quartile) (length probs)

-- Checks if the distribution of a given list for each of the four quartiles is roughly in bound. True if all four quartile are roughly equal to each other.
checkDistribution:: [Float] -> Bool
checkDistribution [] = True
checkDistribution randomList = isQuartileInBound randomList 0.0 && isQuartileInBound randomList 1.0 && isQuartileInBound randomList 2.0 && isQuartileInBound randomList 3.0

-- QuickCheck test if the distribution of the given RNG generates the correct distribution.
-- If this succeeds then the clain that the RNG generates numbers randomly up to a certain level of tolerance.
main :: IO ()
main = do
    quickCheck (checkDistribution (genProbs 10000))