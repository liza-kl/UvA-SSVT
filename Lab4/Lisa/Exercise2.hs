module Exercise2 where
import Exercise1
import SetOrd
import Test.QuickCheck
-- intersection
-- union
-- difference

setIntersection :: Ord a => Set a -> Set a -> Set a
setUnion :: Ord a => Set a -> Set a -> Set a
setDifference :: Ord a => Set a -> Set a -> Set a

setUnion = unionSet

setIntersection (Set []) set2 = Set [] -- If a set is empty you return an empty set (no overlap there)
setIntersection (Set (x:xs)) set2
    | inSet x set2 = insertSet x (setIntersection (Set xs) set2)
    | otherwise = setIntersection (Set xs) set2

setDifference set1 (Set []) = set1
differenceSet set1 (Set (y:ys)) =
    differenceSet (deleteSet y set1) (Set ys)

-- A∩B=B∩A
prop_intersectionCommulative :: (Eq a, Ord a) => Set a -> Set a -> Property
prop_intersectionCommulative set1 set2 = property (setIntersection set1 set2 == setIntersection set2 set1)

-- (A∩B)∩C=A∩(B∩C)
prop_intersectionAssociative :: Set Int -> Set Int -> Set Int -> Property
prop_intersectionAssociative set1 set2 set3 =
    property (setIntersection (setIntersection set1 set2) set3 == setIntersection set1 (setIntersection set2 set3))

-- A∩(B∩C)=(A∩B)∪(A∩C)
prop_intersectionDistributive :: (Eq a, Ord a) => Set a -> Set a -> Set a -> Property
prop_intersectionDistributive set1 set2 set3 = property (setIntersection set1 (setIntersection set2 set3) == setUnion (setIntersection set1 set2) (setIntersection set1 set3))

uniqueIntList :: Gen (Set Int)
uniqueIntList = do
    suchThat generateSets' (\s -> setLength s < 8)

instance Arbitrary (Set Int) where
    arbitrary = uniqueIntList -- Use your existing generator

setToList:: Set a -> [a]
setToList (Set []) = []
setToList (Set (x:xs)) = x : setToList (Set xs)

setLength:: Set a -> Int
setLength set = length (setToList set)

-- ## Testing 
main = do
    print "Testing prop_differenceSameSet"
    -- verboseCheck $ forAll generateSets prop_differenceSameSet
    -- verboseCheck $ forAll generateSets' prop_differenceSameSet
    -- verboseCheck $ forAll generateSets' prop_differenceEmptySet1
    print "Testing prop_unionCommulative"
   -- verboseCheck $ forAll uniqueIntList prop_intersectionCommulative
   -- quickCheckResult $ forAll uniqueIntList prop_intersectionAssociative
    verboseCheck $ forAll uniqueIntList prop_intersectionDistributive
