module Exercise6 where
import SetOrd
import Exercise3
import Data.List
import Test.QuickCheck
-- Test generation for random domains
import Control.Monad

import qualified Exercise5 as Ex5
import Test.QuickCheck (quickCheck)

-- TODO Check Complement and composition func in trash. Maybe add something to the test report
-- symmetric closure: For symmetric closure a reasonbale property to test, if 
-- inverse of each tuple is contained in the relation 

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
    nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- ## Helper Functions 
containedIn :: Ord a => [a] -> [a] -> Bool
containedIn xs ys = all (`elem` ys) xs

inverse :: Rel a -> Rel a
inverse = map (\ (x,y) -> (y,x))

-- https://codereview.stackexchange.com/questions/152190/producing-all-possible-combinations-of-a-list
-- TODO: Can be possibly be deleted as not used in the file 
createGroups :: [a] -> Rel a
createGroups [] = []
createGroups (x:xs) = map (x,) xs ++ createGroups xs

relationToSet :: Ord a => Rel a -> [a]
relationToSet [] = []
relationToSet ((x,y):ps) = sort (nub s)
  where s = x : y : relationToSet ps

setToRelation :: Ord a => [a] -> Rel a
setToRelation xs = [(x, y) | x <- xs, y <- xs, x /= y]


-- Define the range of values for each element in the tuple
-- TODO: Can we define a generator which is more flexible or takes the min max value for the complement?
minValue, maxValue :: Int
minValue = 1
maxValue = 5

-- Calculate the complement of the tuple set
-- For sake of ease only ints
complementTupleSet :: Rel Int -> Rel Int
complementTupleSet rel = [(x, y) | x <- [minValue..maxValue], y <- [minValue..maxValue], (x, y) `notElem` rel]

-- ## Properties 

-- A relation is symmetric, if the inverse of the tuples is included in the relation
prop_containsInverse :: Ord a => Rel a -> Bool
prop_containsInverse r = containedIn (inverse r) (r)

-- If the relation is symmetric, this should also hold for the complement of the
-- relation
prop_complementIsSym :: Rel Int -> Bool
prop_complementIsSym relation = prop_containsInverse relation && prop_containsInverse (complementTupleSet relation)

-- If two different relations are symmetric than the union and intersection 
-- of these two relations must also be symmetric 
prop_twoDistinctRelsSym :: Ord a => Rel a -> Rel a -> Bool
prop_twoDistinctRelsSym rel1 rel2 = prop_containsInverse (setToRelation (relationToSet rel1 `union` relationToSet rel2) )

-- A converse relation is just basically flipping the binary pairs. (Carol, Bob) -> (Bob, Carol)
-- if flipping the pairs in a symmetric relation, the outcome should also be symmetric.
prop_isConverseRelSym :: Ord a => Rel a -> Bool
prop_isConverseRelSym rel = rel == inverse rel

-- initial elements should be in the transitive closure
prop_initialElemsInTransitiveClosure :: Ord a => Rel a ->  Bool
prop_initialElemsInTransitiveClosure initialSet = initialSet `containedIn` Ex5.trClos initialSet

-- Using the infix helper which checks if all transitive pairs are present in a relation
isTransitive :: Ord a => Rel a -> Bool
isTransitive r = containedIn (r @@ r) r

-- is the relation with transitive pairs contained in the given relation  
prop_isTransitive :: Ord a => Rel a -> Bool
prop_isTransitive r = containedIn (r @@ r) r


-- initial elements should be in the symmetric closure
prop_initialElemsInSymmetricClosure :: Ord a => Rel a -> Bool
prop_initialElemsInSymmetricClosure initial = initial `containedIn` symClos initial

-- inverse of initial elements should be in the symmetric closure
prop_inverseOfInitialElemsInSymmetricClosure :: Ord a => Rel a ->  Bool
prop_inverseOfInitialElemsInSymmetricClosure initial =  inverse initial `containedIn` initial

-- in a relation there shouldn't be no duplicates
prop_noDuplicates :: Ord a => Rel a -> Bool
prop_noDuplicates rel = relationToSet rel == nub (relationToSet rel)

-- "Random" Generator for Relations 
-- Give it a list (or in a mathematical context a "Set A")
-- Then give it a function in which you put two values of the given list inside 
-- and if they return "True" they are in a relation 

-- Define a list of possible binary functions
possibleRel :: [Int -> Int -> Bool]
possibleRel = [(==), (<=), (>=), (<), (>),  \x y-> y == x + 1] -- You can extend this list as needed

-- Define a generator for a single element of your relation
genElement :: Gen (Int, Int)
genElement = do
    x <- arbitrary
    y <- arbitrary
    return (x, y)

-- Define a generator for Rel Int
genRel :: Gen (Rel Int)
genRel = do
    func <- elements possibleRel
    elements <- listOf1 genElement
    return $ nub [(x, y) | (x, y) <- elements, func x y] -- Create a unique list which satifies the chosen function 

genSymRel :: Gen (Rel Int)
genSymRel = do
    genRel `suchThat` prop_containsInverse


genTransRel :: Gen (Rel Int)
genTransRel = do
    genRel `suchThat` isTransitive

-- Test Report: All properties pass the test with our generators, though some properties
-- like "contains initial pairs", "prop_noDuplicates" might be superflous because they seem trivial to us
-- (I think the Set type in Haskell does not allow duplicates after all). As everywhere, we are limited to 
-- finite Sets / Relation and can't test it for everyhing. Surprisingly, the prop_twoDistinctRelsSym is the slowest one.

main = do
    print "Testing symmetric closures"
    print "Test Property prop_containsInverse"
    quickCheck $ forAll genSymRel prop_containsInverse
    print "Test Property prop_inverseOfInitialElemsInSymmetricClosure"
    quickCheck $ forAll genSymRel prop_inverseOfInitialElemsInSymmetricClosure
    print "Test Property prop_complementIsSym"
    quickCheck $ forAll genSymRel prop_complementIsSym
    print "Test Property prop_initialElemsInSymmetricClosure"
    quickCheck $ forAll genRel prop_initialElemsInSymmetricClosure
    print "Test Property prop_twoDistinctRelsSym"-- This is extremly slow in terms of tests
    quickCheck $ forAll genSymRel prop_twoDistinctRelsSym
    print "Test Property prop_isConverseRelSym"
    quickCheck $ forAll genSymRel prop_isConverseRelSym
    print "Checking if there are no duplicates in symmetric closure"
    quickCheck $ forAll genSymRel prop_noDuplicates

    print "Checking if initial elements of the relation are in the transitive closure"
    quickCheck $ forAll genTransRel prop_initialElemsInTransitiveClosure
    print "Checking if all necessary transitive pairs are in the transitive closure"
    quickCheck $ forAll genTransRel prop_isTransitive
    print "Checking if there are no duplicates in transitive closure"
    quickCheck $ forAll genTransRel prop_noDuplicates


-- Trash 

-- Maybe also a prop for trClose  use page 191 in Haskell Book.
-- If  𝑟
  -- is a relation on a set  𝐴
  -- and  |𝐴|=𝑛,
  -- then the transitive closure of  𝑟
  -- is the union of the first  𝑛
  -- powers of r. 

-- Define your property function
-- prop_trClosPowerOf rel closure power = 
--           until (power == 0) (prop_trClosPowerOf rel (unionSet rel) (power - 1))
--           closure == (Ex5.trClos rel)

-- Composing relations in order to proof transitivity. 
-- implementation taken from Haskell Road to Logic pg. 180

--TODO: pg. 180 in Haskell Road Book.
-- composePair ::Ord a=>(a,a)->Rel a->Rel a
-- composePair (x,y) ([]) = [] -- if there is an empty relation, no composition is possible since no middle elem is there 
-- composePair (x,y) (((u,v):s)) -- for the recursion you look at the pair to compose with each of the tuple pairs in the relatio 
-- -- -- if the last elem of the first tuple equals the first elem of the snd tuple, insert it into the set 
--                   | y==u =insertSet (x,v) (composePair (x,y) (setToRelation s))
--                   | otherwise =composePair (x,y) (setToRelation s)

-- compR ::Ord a=>Rel a->Rel a->Rel a
-- compR (Set []) _= (Set [])
-- compR (Set ((x,y):s)) r= unionSet (composePair (x,y) r)(compR (setToRelation s)r)

-- repeatR ::Ord a=>Rel a->Int ->Rel a
-- repeatR r n
--         |n<1 =error "argument <1"
--         |n==1 =r
--         |otherwise =compR r(repeatR r(n-1))