module Exercise6 where
import SetOrd
import Data.List
-- Test generation for random domains

-- symmetric closure: For symmetric closure a reasonbale property to test, if 
-- inverse of each tuple is contained in the relation 
type Rel a = [(a, a)]

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

prop_containsInverse :: Ord a => Rel a -> Bool
prop_containsInverse r = containedIn (inverse r) r

-- If the relation is symmetric, this should also hold for the complement of the
-- relation (Kreuzprodukt intersect die ursprüngliche Menge)
-- Example [(1, 2), (2, 1)]
prop_complementIsSym :: Rel Int -> Bool
prop_complementIsSym relation = prop_containsInverse relation && prop_containsInverse (complementTupleSet relation)

-- If two different relations are symmetric than the union and intersection 
-- of these two relations must also be symmetric 
-- There is somewhere a bug. 
prop_twoDistinctRelsSym :: Ord a => Rel a -> Rel a -> Bool
prop_twoDistinctRelsSym rel1 rel2 = prop_containsInverse  (setToRelation (relationToSet rel1 `union` relationToSet rel2) )


calcConverse :: Ord a => Rel a -> Rel a 
calcConverse [] = [] -- base case empty relation 
-- Die konverse Relation muss der ursprünglichen Relation ähneln
-- The converse of a relation must equal the initial relation 
prop_isConverseRelSym :: Ord a => Rel a -> Bool 
prop_isConverseRelSym rel = rel == sort (inverse rel )

-- even number of elements in symmetric closure, since it contains the initial elements and their inverse
prop_evenElemsInSymmetric :: Ord a => Rel a -> Bool
prop_evenElemsInSymmetric relation = (mod (length relation) 2 == 0)

-- initial elements should be in the transitive closure
prop_initialElemsInTransitiveClosure :: Ord a => Rel a -> Rel a -> Bool
prop_initialElemsInTransitiveClosure initial transitive = containedIn initial transitive

-- initial elements should be in the symmetric closure
prop_initialElemsInSymmetricClosure :: Ord a => Rel a -> Rel a -> Bool
prop_initialElemsInSymmetricClosure initial symmetric = containedIn initial symmetric

-- inverse of initial elements should be in the symmetric closure
prop_inverseOfInitialElemsInSymmetricClosure :: Ord a => Rel a -> Rel a -> Bool
prop_inverseOfInitialElemsInSymmetricClosure initial symmetric = containedIn (inverse initial) symmetric