module Exercise6 where
import SetOrd
import Exercise3
import Data.List
import Test.QuickCheck
-- Test generation for random domains
import Control.Monad
import Debug.Trace
import qualified Exercise5 as Ex5
-- TODO Add random test generation (prop. with generators from Ex 1)
-- TODO Test Report
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

trClos' relation = nub $ [x | (x, _) <- relation] ++ [y | (_, y) <- relation]

-- https://codereview.stackexchange.com/questions/152190/producing-all-possible-combinations-of-a-list
createGroups :: [a] -> Rel a
createGroups [] = []
createGroups (x:xs) = map (x,) xs ++ createGroups xs

relationToSet :: Ord a => Rel a -> [a]
relationToSet [] = []
relationToSet ((x,y):ps) = sort (nub s)
  where s = x : y : relationToSet ps

setToRelation :: Ord a => [a] -> Rel a
setToRelation xs = [(x, y) | x <- xs, y <- xs, x /= y]

-- Define a list of tuples (you can replace this with your own set of pairs)
tupleSet :: [(Int, Int)]
tupleSet = [(1, 2), (2, 3), (4, 5)]

-- Define the range of values for each element in the tuple
minValue, maxValue :: Int
minValue = 1
maxValue = 5

-- Calculate the complement of the tuple set
-- For sake of ease only ints
complementTupleSet :: Rel Int -> Rel Int
complementTupleSet rel = [(x, y) | x <- [minValue..maxValue], y <- [minValue..maxValue], (x, y) `notElem` rel]


calcConverse :: Ord a => Rel a -> Rel a
calcConverse [] = [] -- base case empty relation 
-- Die konverse Relation muss der ursprünglichen Relation ähneln
-- The converse of a relation must equal the initial relation 

-- ## Properties 

-- A relation is symmetric, if the inverse of the tuples is included in the relation
prop_containsInverse :: Ord a => Rel a -> Bool
prop_containsInverse r = containedIn (inverse r) (r)

-- If the relation is symmetric, this should also hold for the complement of the
-- relation
prop_complementIsSym :: Rel Int -> Bool
prop_complementIsSym relation = prop_containsInverse (relation) && prop_containsInverse (complementTupleSet relation)

-- If two different relations are symmetric than the union and intersection 
-- of these two relations must also be symmetric 
-- There is somewhere a bug. 
prop_twoDistinctRelsSym :: Ord a => Rel a -> Rel a -> Bool
prop_twoDistinctRelsSym rel1 rel2 = prop_containsInverse (setToRelation (relationToSet rel1 `union` relationToSet rel2) )

-- A converse relation is just basically flipping the binary pairs. (Carol, Bob) -> (Bob, Carol)
prop_isConverseRelSym :: Ord a => Rel a -> Bool
prop_isConverseRelSym rel = rel == sort (inverse rel)

-- initial elements should be in the transitive closure
prop_initialElemsInTransitiveClosure :: Ord a => Rel a ->  Bool
prop_initialElemsInTransitiveClosure initialSet = initialSet `containedIn` Ex5.trClos initialSet

-- initial elements should be in the symmetric closure
prop_initialElemsInSymmetricClosure :: Ord a => Rel a -> Bool
prop_initialElemsInSymmetricClosure initial = initial `containedIn` symClos initial

-- inverse of initial elements should be in the symmetric closure
prop_inverseOfInitialElemsInSymmetricClosure :: Ord a => Rel a ->  Bool
prop_inverseOfInitialElemsInSymmetricClosure initial =  inverse initial `containedIn` initial


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


isTransitive :: Ord a => Rel a -> Bool
isTransitive r = containedIn (r @@ r) r

genTransRel :: Gen (Rel Int)
genTransRel = do
    genRel `suchThat` isTransitive


-- uniqueIntList :: Gen [Int]
-- uniqueIntList = do
--     set <- suchThat generateSets' (\s -> setLength s >= 3 && setLength s < 8)
--     let setList = setToList set
--     return setList



main = do
    print "Test Property prop_containsInverse"
    quickCheck $ forAll genSymRel prop_containsInverse
    print "Test Property prop_complementIsSym"
    quickCheck $ forAll genSymRel prop_complementIsSym
    print "Test Property prop_twoDistinctRelsSym"
    quickCheck $ forAll genSymRel prop_twoDistinctRelsSym
    print "Test Property prop_isConverseRelSym"
    quickCheck $ forAll genSymRel prop_isConverseRelSym

  --   print "Test Property prop_twoDistinctRelsSym" -- This is extremly slow in terms of tests
  --  -- quickCheck $ forAll genRel prop_twoDistinctRelsSym
  --   print "Test Property prop_initialElemsInTransitiveClosure"
  --   quickCheck $ forAll genRel prop_initialElemsInTransitiveClosure
  --   print "Test Property prop_initialElemsInSymmetricClosure"
  --   quickCheck $ forAll genRel prop_initialElemsInSymmetricClosure
  --   print "Test Property prop_inverseOfInitialElemsInSymmetricClosure"
  --   quickCheck $ forAll genRel prop_inverseOfInitialElemsInSymmetricClosure



-- Trash 
-- even number of elements in symmetric closure, since it contains the initial elements and their inverse
-- prop_evenElemsInSymmetric :: Ord a => Rel a ->  Bool
-- prop_evenElemsInSymmetric relation = mod (length relation) 2 == 0

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
