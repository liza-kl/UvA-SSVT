module Testing where

import Test.QuickCheck
import Data.List

testOrdered :: OrderedList Int -> Bool
testOrdered _ = True

-- verboseCheck testNonEmpty
testNonEmpty :: NonEmptyList (Positive Int) -> Bool
testNonEmpty xs = length xs'' > 0
  where
    xs' = getNonEmpty xs
    xs'' = map getPositive xs'

testNonZero :: NonZero Int -> Bool
testNonZero _ = True

testFixed :: Fixed Int -> Bool
testFixed (Fixed x) = True

{--
OrderedList
NonEmptyList
Positive
NonZero
NonNegative
Large
Small
Fixed
--}

-- Generator

newtype Set' a = Set' [a] deriving (Eq,Ord)

list2set' :: Ord a => [a] -> Set' a
list2set' xs = Set' (sort (nub xs))

instance (Ord a, Arbitrary a) => Arbitrary (Set' a) where
    arbitrary = do
            xs <- arbitrary
            return (list2set' xs)

randomIntSetGenerator :: Gen (Set' Int)
randomIntSetGenerator = do
    list <- listOf arbitrary
    return (list2set' list)

randomIntSets :: IO (Set' Int)
randomIntSets = generate (randomIntSetGenerator::Gen (Set' Int))

qcFunc1 :: Set' Int -> Bool
qcFunc1 _ = True

-- Arbitrary type

newtype Vowel = Vowel String deriving (Eq,Ord,Show)

instance Arbitrary Vowel where
  arbitrary = oneof (map (return.Vowel) ["a", "e", "i", "o", "u"])

testVowelList :: [Vowel] -> Bool
testVowelList vs = True