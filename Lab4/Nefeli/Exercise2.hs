module Exercise2 where
import Test.QuickCheck
import Data.List
import System.Random
import SetOrd
import Exercise1

setUnion :: Ord a => Set a -> Set a -> Set a
setUnion set1 set2 = unionSet set1 set2

-- A∪B=B∪A
prop_unionCommulative :: (Eq a, Ord a) => Set a -> Set a -> Property
prop_unionCommulative set1 set2 = property (setUnion set1 set2 == setUnion set2 set1) 

-- (A∪B)∪C=A∪(B∪C)
prop_unionAssociative :: (Eq a, Ord a) => Set a -> Set a -> Set a -> Property
prop_unionAssociative set1 set2 set3 = property (setUnion (setUnion set1 set2) set3 == setUnion set1 (setUnion set2 set3))

-- A∪A=A
prop_unionIdempotent :: (Eq a, Ord a) => Set a -> Property
prop_unionIdempotent set1 = property (setUnion set1 set1 == set1)

-- A - B
-- if A is empty, A - B is empty
-- if B is empty, A - B is A
-- if none is empty, check if first element of A is in B
-- if it is, then it shouldn't be included in A - B
-- so call function recursively with the rest elements as A set
-- if it is not, it should be added to the result and then
-- call fuctnion recursively with the rest elements as A set
setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set []) _ = emptySet
setDifference set1 (Set []) = set1
setDifference (Set (x:xs)) set2
  | inSet x set2 = setDifference (Set xs) set2
  | otherwise = insertSet x (setDifference (Set xs) set2)

prop_differenceEmptySet1 :: (Eq a, Ord a) => Set a -> Set a -> Property
prop_differenceEmptySet1 set1 set2 = property (setDifference set1 set2 == emptySet)

prop_differenceEmptySet2 :: (Eq a, Ord a) => Set a -> Set a -> Property
prop_differenceEmptySet2 set1 set2 = property (setDifference set1 set2 == set1)

prop_differenceSameSet :: (Eq a, Ord a) => Set a -> Property
prop_differenceSameSet set1 = property (setDifference set1 set1 == emptySet)

-- A ∩ B
-- if A or B is empty, A ∩ B is empty
-- if none is empty, if first element of A is in B, add it to the result
-- so call function recursively with the rest elements as A set
-- otherwise, it should not be inclued in A ∩ B, 
-- so call function recursively with the rest elements as A set
setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection (Set []) _ = emptySet
setIntersection _ (Set []) = emptySet
setIntersection (Set (x:xs)) set2
  | inSet x set2 = insertSet x (setIntersection (Set xs) set2)
  | otherwise = setIntersection (Set xs) set2

-- A∩B=B∩A
prop_intersectionCommulative :: (Eq a, Ord a) => Set a -> Set a -> Property
prop_intersectionCommulative set1 set2 = property (setIntersection set1 set2 == setIntersection set2 set1) 

-- (A∩B)∩C=A∩(B∩C)
prop_intersectionAssociative :: (Eq a, Ord a) => Set a -> Set a -> Set a -> Property
prop_intersectionAssociative set1 set2 set3 = property (setIntersection (setIntersection set1 set2) set3 == setIntersection set1 (setIntersection set2 set3))

-- A∩(B∩C)=(A∩B)∪(A∩C)
prop_intersectionDistributive :: (Eq a, Ord a) => Set a -> Set a -> Set a -> Property
prop_intersectionDistributive set1 set2 set3 = property (setIntersection set1 (setIntersection set2 set3) == setUnion (setIntersection set1 set2) (setIntersection set1 set3))
