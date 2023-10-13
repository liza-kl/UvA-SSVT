module Exercise2 where
import Test.QuickCheck
import Data.List
import System.Random
import SetOrd
import Exercise1

-- ## Deliverables
-- implementations, test properties, short test report, indication of time spent.

-- ## Indication of time spent 145 minutes 

-- Took the unionSet implementation from provided "SetOrd".
-- Wanted to follow the DRY (Don't Repeat Yourself) principle in this 
-- and not provide another implementation when there is one already present.

{-

  The "Road To Haskell" book provides the following implementation. (pg. 191)

  unionSet ::(Ord a) => Set a -> Set a -> Set a 
  unionSet (Set []) set2 = set2
  unionSet (Set (x:xs)) set2 = insertSet x(unionSet (Set xs) (deleteSet xset2)))

  Honestly, we weren't sure why a "deleteSet" was included.
  (Maybe performance reasons???)

-}

setUnion :: Ord a => Set a -> Set a -> Set a
setUnion = unionSet

-- For the different properties we are using tuples since this makes providing the set values easier.
-- With this we only have one value and we can use the derived (see below) generator functions to test the properties.

-- A∪B=B∪A
prop_unionCommutative :: (Eq a, Ord a) => (Set a, Set a) -> Bool
prop_unionCommutative (set1, set2) =  setUnion set1 set2 == setUnion set2 set1

-- (A∪B)∪C=A∪(B∪C)
prop_unionAssociative :: (Eq a, Ord a) => (Set a, Set a, Set a) -> Bool
prop_unionAssociative (set1, set2, set3) =  setUnion (setUnion set1 set2) set3 == setUnion set1 (setUnion set2 set3)

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
-- call function recursively with the rest elements as A set
setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set []) _ = emptySet
setDifference set1 (Set []) = set1
setDifference (Set (x:xs)) set2
  | inSet x set2 = setDifference (Set xs) set2
  | otherwise = insertSet x (setDifference (Set xs) set2)

-- Checks if the difference of two equal equals an empty set. (But with 2 params)
prop_differenceEmptySet1 :: (Eq a, Ord a) => (Set a, Set a) -> Bool
prop_differenceEmptySet1 (set1, set2) = setDifference set1 set2 == emptySet

-- Checks if the difference of any given set and an empty set is the original given set.
prop_differenceEmptySet2 :: (Eq a, Ord a) => (Set a, Set a) -> Bool
prop_differenceEmptySet2 (set1, set2) = setDifference set1 set2 == set1

-- Checks if the difference of the same set equals an empty set.
prop_differenceSameSet :: (Eq a, Ord a) => Set a -> Bool
prop_differenceSameSet set1 = setDifference set1 set1 == emptySet

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

{-
  We introduce a min/max value since in theory our domain would reach from -2147483647 to 2147483647. (Neg to Pos. Integer limit)
  This would create massive compliment lists though and due to preserving computational power we introduce this arbitrary "domain size".
-}
complementIntSet :: Set Int -> Int -> Int -> Set Int
complementIntSet set minValue maxValue = Set [x | x <- [minValue..maxValue], not (inSet x set)]

-- A∩B=B∩A
prop_intersectionCommutative :: (Eq a, Ord a) => (Set a, Set a) -> Bool
prop_intersectionCommutative (set1, set2) =  setIntersection set1 set2 == setIntersection set2 set1

-- (A∩B)∩C=A∩(B∩C)
prop_intersectionAssociative :: (Ord a) => (Set a, Set a, Set a) -> Bool
prop_intersectionAssociative (set1, set2, set3) =
     setIntersection (setIntersection set1 set2) set3 == setIntersection set1 (setIntersection set2 set3)

-- A∩(B∪C)=(A∩B)∪(A∩C)
prop_intersectionDistributive :: (Eq a, Ord a) => (Set a, Set a, Set a) -> Bool
prop_intersectionDistributive (set1, set2, set3) =  setIntersection set1 (setUnion set2 set3) == setUnion (setIntersection set1 set2) (setIntersection set1 set3)

-- A∪(B∩C)=(A∪B)∩(A∪C)
prop_unionDistributive :: (Eq a, Ord a) => (Set a, Set a, Set a) -> Bool
prop_unionDistributive (set1, set2, set3) =  setUnion set1 (setIntersection set2 set3) == setIntersection (setUnion set1 set2) (setUnion set1 set3)


prop_absorptionLaw :: (Eq a, Ord a) => (Set a, Set a) -> Bool
prop_absorptionLaw (set1, set2) = setIntersection set1 (setUnion set1 set2) == set1 && setUnion set1 (setIntersection set1 set2) == set1

prop_intersectionInDifference ::  (Eq a, Ord a) => (Set a, Set a) -> Bool
prop_intersectionInDifference (set1, set2) = setIntersection set1 set2 == setDifference set1 (setDifference set1 set2)

-- Demorgan's laws are a set of two postulates that are widely used in set theory.
-- They state that: (i) (A ∪ B)’ = A’ ∩ B’ and (ii) (A ∩ B)’ = A’ ∪ B’.

prop_firstMorganLaw ::  (Set Int, Set Int, Int, Int) -> Bool
prop_firstMorganLaw (set1, set2, minValue, maxValue) = complementIntSet (setIntersection set1 set2) minValue maxValue == setUnion (complementIntSet set1 minValue maxValue) (complementIntSet set2 minValue maxValue)
prop_secondMorganLaw :: (Set Int, Set Int, Int, Int) -> Bool
prop_secondMorganLaw (set1, set2, minValue, maxValue) = complementIntSet (setUnion set1 set2) minValue maxValue == setIntersection (complementIntSet set1 minValue maxValue) (complementIntSet set2 minValue maxValue)

-- We are providing multiple versions of the two versions of the Ex. 1 QuickCheck generators.
-- This is to circumvent the QuickCheck limitation of only being able to supply one "object" with the generator.
-- So we are using the Ex. 1 generators to generate the corresponding tuples that we need.
-- Also we are restricting the output to eight elements set elements, due to performance.
-- Eight was chosen since it was just a number small enough to easily compute everything while providing decent test coverage for different cases.
quickCheckGen :: Gen (Set Int)
quickCheckGen = do
                  genSet <- generateSets'
                  let reducedSet = take 8 (setToList genSet)
                  return (list2set reducedSet)

quickCheckGenTuple:: Gen (Set Int, Set Int)
quickCheckGenTuple = do
                  g1 <- quickCheckGen
                  g2 <- quickCheckGen
                  return (g1,g2)

quickCheckGenDeMorganTuple:: Gen (Set Int, Set Int, Int, Int)
quickCheckGenDeMorganTuple = do
                          g1 <- quickCheckGen
                          g2 <- quickCheckGen
                          r1 <- choose(1,100)
                          r2 <- choose(1,100)
                          return (g1,g2, min r1 r2, max r1 r2)

-- Explaination for this, see below. Special case generator for "prop_differenceEmptySet1".
quickCheckGenEqualTuple:: Gen (Set Int, Set Int)
quickCheckGenEqualTuple = do
                  g <- quickCheckGen
                  return (g, g)

-- Explaination for this, see below. Special case generator for "prop_differenceEmptySet2".
quickCheckGenHalfEmptyTuple:: Gen (Set Int, Set Int)
quickCheckGenHalfEmptyTuple = do
                  g <- quickCheckGen
                  return (g, emptySet)

quickCheckGenTriple:: Gen (Set Int, Set Int, Set Int)
quickCheckGenTriple = do
                  g1 <- quickCheckGen
                  g2 <- quickCheckGen
                  g3 <- quickCheckGen
                  return (g1, g2, g3)

noQuickCheckGen :: Gen (Set Int)
noQuickCheckGen = do
                    genSet <- generateSetsWithFixedSize
                    let reducedSet = take 8 (setToList genSet)
                    return (list2set reducedSet)

noQuickCheckGenTuple:: Gen (Set Int, Set Int)
noQuickCheckGenTuple = do
                  g1 <- noQuickCheckGen
                  g2 <- noQuickCheckGen
                  return (g1,g2)

noQuickCheckGenDeMorganTuple:: Gen (Set Int, Set Int, Int, Int)
noQuickCheckGenDeMorganTuple = do
                          g1 <- noQuickCheckGen
                          g2 <- noQuickCheckGen
                          r1 <- choose(1,100)
                          r2 <- choose(1,100)
                          return (g1,g2, min r1 r2, max r1 r2)

-- Explaination for this, see below. Special case generator for "prop_differenceEmptySet1".
noQuickCheckGenEqualTuple:: Gen (Set Int, Set Int)
noQuickCheckGenEqualTuple = do
                  g <- noQuickCheckGen
                  return (g, g)

-- Explaination for this, see below. Special case generator for "prop_differenceEmptySet2".
noQuickCheckGenHalfEmptyTuple:: Gen (Set Int, Set Int)
noQuickCheckGenHalfEmptyTuple = do
                  g <- noQuickCheckGen
                  return (g, emptySet)

noQuickCheckGenTriple:: Gen (Set Int, Set Int, Set Int)
noQuickCheckGenTriple = do
                  g1 <- noQuickCheckGen
                  g2 <- noQuickCheckGen
                  g3 <- noQuickCheckGen
                  return (g1, g2, g3)

-- Helper functions for set manipulation.
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
    quickCheckResult $ forAll noQuickCheckGen prop_unionIdempotent
    quickCheckResult $ forAll quickCheckGen prop_unionIdempotent

    print "Testing prop_intersectionIdempotent"
    quickCheckResult $ forAll noQuickCheckGen prop_intersectionIdempotent
    quickCheckResult $ forAll quickCheckGen prop_intersectionIdempotent

    print "Testing Commutitavity. Can be proven with Venn Diagramms or Truth Tables"
    print "Testing prop_unionCommutative"
    quickCheckResult $ forAll noQuickCheckGenTuple prop_unionCommutative
    quickCheckResult $ forAll quickCheckGenTuple prop_unionCommutative
    print "Testing prop_intersectionCommutative"
    quickCheckResult $ forAll noQuickCheckGenTuple prop_intersectionCommutative
    quickCheckResult $ forAll noQuickCheckGenTuple prop_intersectionCommutative

    print "Testing Associativity. Can be proven with Venn Diagramms or Truth Tables"
    print "Testing prop_unionAssociative"
    quickCheckResult $ forAll noQuickCheckGenTriple prop_unionAssociative
    quickCheckResult $ forAll quickCheckGenTriple prop_unionAssociative
    print "Testing prop_intersectionAssociative"
    quickCheckResult $ forAll noQuickCheckGenTriple prop_intersectionAssociative
    quickCheckResult $ forAll quickCheckGenTriple prop_intersectionAssociative

    print "Testing Distributivity. Can be proven with Venn Diagramms or Truth Tables"
    print "Testing prop_intersectionDistributive"
    quickCheckResult $ forAll noQuickCheckGenTriple prop_intersectionDistributive
    quickCheckResult $ forAll quickCheckGenTriple prop_intersectionDistributive
    print "Testing prop_unionDistributive"
    quickCheckResult $ forAll noQuickCheckGenTriple prop_unionDistributive
    quickCheckResult $ forAll quickCheckGenTriple prop_unionDistributive

    print "Testing setDifferenceFunction. Can be proven with Venn Diagramms or Truth Tables"
    print "Testing prop_differenceSameSet"
    quickCheckResult $ forAll noQuickCheckGen prop_differenceSameSet
    quickCheckResult $ forAll quickCheckGen prop_differenceSameSet

-- Needs the special generator, which provides a tuple with the same sets.
-- This is since we test here if a subtraction of those elements would result in an empty set.
    print "Testing prop_differenceEmptySet1"
    quickCheckResult $ forAll noQuickCheckGenEqualTuple prop_differenceEmptySet1
    quickCheckResult $ forAll quickCheckGenEqualTuple prop_differenceEmptySet1

-- Needs the special generator, which provides a tuple with a normal set element and an empty set.
-- This is since we test here if a subtraction of those elements would result in the original set.
    print "Testing prop_differenceEmptySet2"
    quickCheckResult $ forAll noQuickCheckGenHalfEmptyTuple prop_differenceEmptySet2
    quickCheckResult $ forAll quickCheckGenHalfEmptyTuple prop_differenceEmptySet2

    print "Testing prop_differenceSameSet"
    quickCheckResult $ forAll noQuickCheckGen prop_differenceSameSet
    quickCheckResult $ forAll quickCheckGen prop_differenceSameSet

    print "Testing prop_absorptionLaw"
    quickCheckResult $ forAll noQuickCheckGenTuple prop_absorptionLaw
    quickCheckResult $ forAll quickCheckGenTuple prop_absorptionLaw

    print "Testing prop_intersectionInDifference"
    quickCheckResult $ forAll noQuickCheckGenTuple prop_intersectionInDifference
    quickCheckResult $ forAll quickCheckGenTuple prop_intersectionInDifference

    print "Testing first DeMorgan law"
    quickCheckResult $ forAll noQuickCheckGenDeMorganTuple prop_firstMorganLaw
    quickCheckResult $ forAll quickCheckGenDeMorganTuple prop_firstMorganLaw

    print "Testing second DeMorgan law"
    quickCheckResult $ forAll noQuickCheckGenDeMorganTuple prop_secondMorganLaw
    quickCheckResult $ forAll quickCheckGenDeMorganTuple prop_secondMorganLaw

{-

    Test report:

    Generally all properties do work as expected.
    The only restriction that we are currently facing is the fact,
    that we can't test with infinite sets.
    Therefore to prove for those cases you would need to provide mathematical proofs.
    For example via Venn Diagrams or Logical Proofs.

    Probably some properties could be left out, 
    due to being implicitly included in other properties.
    We left that as is though, for better visibility and a better property showcase.

-}