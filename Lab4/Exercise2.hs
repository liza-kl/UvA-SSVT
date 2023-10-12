module Exercise2 where
import Test.QuickCheck
import Data.List
import System.Random
import SetOrd
import Exercise1
import Data.Sequence.Internal.Sorting (Queue(Q))

-- ## Deliverables
-- implementations, test properties, short test report, indication of time spent.

-- ## Indication of time spent 80 minutes 
setUnion :: Ord a => Set a -> Set a -> Set a
setUnion = unionSet

-- A∪B=B∪A
prop_unionCommutative :: (Eq a, Ord a) => Set a -> Set a -> Bool
prop_unionCommutative set1 set2 =  setUnion set1 set2 == setUnion set2 set1

-- (A∪B)∪C=A∪(B∪C)
prop_unionAssociative :: (Eq a, Ord a) => Set a -> Set a -> Set a -> Bool
prop_unionAssociative set1 set2 set3 =  setUnion (setUnion set1 set2) set3 == setUnion set1 (setUnion set2 set3)

-- A∪A=A
prop_unionIdempotent :: (Eq a, Ord a) => Set a -> Bool
prop_unionIdempotent set1 =  setUnion set1 set1 == set1

prop_intersectionIdempotent :: (Eq a, Ord a) => Set a -> Bool
prop_intersectionIdempotent set1 = setIntersection set1 set1 == set1

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


prop_differenceEmptySet1 :: (Eq a, Ord a) => Set a -> Set a -> Bool
prop_differenceEmptySet1 set1 set2 =  setDifference set1 set2 == emptySet

prop_differenceEmptySet2 :: (Eq a, Ord a) => Set a -> Set a -> Bool
prop_differenceEmptySet2 set1 set2 =  setDifference set1 set2 == set1

prop_differenceSameSet :: (Eq a, Ord a) => Set a -> Bool
prop_differenceSameSet set1 =  setDifference set1 set1 == emptySet

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
prop_intersectionCommutative :: (Eq a, Ord a) => Set a -> Set a -> Bool
prop_intersectionCommutative set1 set2 =  setIntersection set1 set2 == setIntersection set2 set1

-- (A∩B)∩C=A∩(B∩C)
prop_intersectionAssociative :: Set Int -> Set Int -> Set Int -> Bool
prop_intersectionAssociative set1 set2 set3 =
     setIntersection (setIntersection set1 set2) set3 == setIntersection set1 (setIntersection set2 set3)

-- A∩(B∪C)=(A∩B)∪(A∩C)
prop_intersectionDistributive :: (Eq a, Ord a) => Set a -> Set a -> Set a -> Bool
prop_intersectionDistributive set1 set2 set3 =  setIntersection set1 (setUnion set2 set3) == setUnion (setIntersection set1 set2) (setIntersection set1 set3)

-- A∪(B∩C)=(A∪B)∩(A∪C)
prop_unionDistributive :: (Eq a, Ord a) => Set a -> Set a -> Set a -> Bool
prop_unionDistributive set1 set2 set3 =  setUnion set1 (setIntersection set2 set3) == setIntersection (setUnion set1 set2) (setUnion set1 set3)


prop_absorptionLaw :: (Eq a, Ord a) => Set a -> Set a -> Bool
prop_absorptionLaw set1 set2 = setIntersection set1 (setUnion set1 set2) == set1 && setUnion set1 (setIntersection set1 set2) == set1

prop_intersectionInDifference ::  (Eq a, Ord a) => Set a -> Set a -> Bool
prop_intersectionInDifference set1 set2 = setIntersection set1 set2 == setDifference set1 (setDifference set1 set2)

-- Demorgan's laws are a set of two postulates that are widely used in set theory.
-- They state that: (i) (A ∪ B)’ = A’ ∩ B’ and (ii) (A ∩ B)’ = A’ ∪ B’.

prop_firstMorganLaw ::  (Eq a, Ord a) => Set a -> Set a -> Bool
prop_firstMorganLaw = undefined -- TODO Implement
prop_secondMorganLaw ::  (Eq a, Ord a) => Set a -> Set a -> Bool
prop_secondMorganLaw set1 set2 = undefined -- TODO Implement

uniqueIntList :: Gen (Set Int)
uniqueIntList = suchThat generateSets' (\s -> setLength s < 8)

noQuickCheck :: Gen (Set Int)
noQuickCheck = suchThat generateSets (\s -> setLength s < 8)


setToList:: Set a -> [a]
setToList (Set []) = []
setToList (Set (x:xs)) = x : setToList (Set xs)

setLength:: Set a -> Int
setLength set = length (setToList set)

-- ## Testing 
-- Properties were chosen based on the Theorem 4.16 from the Haskell Road to Logic Book.
-- We can derive from that that if all of our functions are correctly implemented then the
-- laws of idempotence, commutativity, associativity, distributivity, De Morgan's Laws should result in true.
--
-- Writing all the stuff down which was proven by many intelligent people before us would be in our sense kind of pointless,
-- so we just say that we can prove it with Venn Diagramms or Truth Tables. 
-- Although, the properties seem kind of complete, we are limited to the fact, that we can't test infinite sets. So a precondition
-- for a Bool tests are fixed-sized sets. 
-- Maybe we can prove by induction that if it holds for n, it holds for n+1 ?


main = do
    print "Testing Idempotence. Can be proven with Venn Diagramms or Truth Tables"
    print "Testing prop_unionIdempotent"
    quickCheckResult $ forAll noQuickCheck prop_unionIdempotent
    quickCheckResult $ forAll uniqueIntList prop_unionIdempotent

    print "Testing prop_intersectionIdempotent"
    quickCheckResult $ forAll noQuickCheck prop_intersectionIdempotent
    quickCheckResult $ forAll uniqueIntList prop_intersectionIdempotent

    print "Testing Commutitavity. Can be proven with Venn Diagramms or Truth Tables"
    print "Testing prop_unionCommutative"
    quickCheckResult $ forAll noQuickCheck prop_unionCommutative
    quickCheckResult $ forAll uniqueIntList prop_unionCommutative
    print "Testing prop_intersectionCommutative"
    quickCheckResult $ forAll noQuickCheck prop_intersectionCommutative
    quickCheckResult $ forAll uniqueIntList prop_intersectionCommutative

    print "Testing Associativity. Can be proven with Venn Diagramms or Truth Tables"
    print "Testing prop_unionAssociative"
    quickCheckResult $ forAll noQuickCheck prop_unionAssociative
    quickCheckResult $ forAll uniqueIntList prop_unionAssociative
    print "Testing prop_intersectionAssociative"
    quickCheckResult $ forAll noQuickCheck prop_intersectionAssociative
    quickCheckResult $ forAll uniqueIntList prop_intersectionAssociative

    print "Testing Distributivity. Can be proven with Venn Diagramms or Truth Tables"
    print "Testing prop_intersectionDistributive"
    quickCheckResult $ forAll noQuickCheck prop_intersectionDistributive
    quickCheckResult $ forAll uniqueIntList prop_intersectionDistributive
    print "Testing prop_unionDistributive"
    quickCheckResult $ forAll noQuickCheck prop_unionDistributive
    quickCheckResult $ forAll uniqueIntList prop_unionDistributive

    print "Testing setDifferenceFunction. Can be proven with Venn Diagramms or Truth Tables"
    print "Testing prop_differenceSameSet"
    quickCheckResult $ forAll noQuickCheck prop_differenceSameSet
    quickCheckResult $ forAll uniqueIntList prop_differenceSameSet
    print "Testing prop_differenceEmptySet1"
    quickCheckResult $ forAll noQuickCheck prop_differenceEmptySet1
    quickCheckResult $ forAll uniqueIntList prop_differenceEmptySet1
    print "Testing prop_differenceEmptySet2"
    quickCheckResult $ forAll noQuickCheck prop_differenceEmptySet2
    quickCheckResult $ forAll uniqueIntList prop_differenceEmptySet2
    print "Testing prop_differenceSameSet"
    quickCheckResult $ forAll noQuickCheck prop_differenceSameSet
    quickCheckResult $ forAll uniqueIntList prop_differenceSameSet

    print "Testing prop_absorptionLaw"
    quickCheckResult $ forAll noQuickCheck prop_absorptionLaw
    quickCheckResult $ forAll uniqueIntList prop_absorptionLaw
    print "Testing prop_intersectionInDifference"
    quickCheckResult $ forAll noQuickCheck prop_intersectionInDifference
    quickCheckResult $ forAll uniqueIntList prop_intersectionInDifference








