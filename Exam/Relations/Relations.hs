module Relations where
import Data.List
import qualified Data.Set as Set 

type Rel a = [(a, a)] -- A Relation is a tuple of a type

relationToSet :: Ord a => Rel a -> Set.Set (a, a)
relationToSet [] = Set.empty
relationToSet ((x, y):ps) = Set.insert (x, y) (relationToSet ps)

setToRelation :: Ord a => [a] -> Rel a
setToRelation xs = [(x, y) | x <- xs, y <- xs, x /= y]

composeRelations :: Eq a => Rel a -> Rel a -> Rel a
composeRelations r s = [(a, b) | (a, c) <- r, (c, b) <- s]


inverse :: Rel a -> Rel a
inverse = map (\ (x,y) -> (y,x))

symmetricClosure :: Ord a => Rel a -> Rel a
symmetricClosure r = sort ( nub (r ++ inverse r) )

-- Relation composition
infixr 5 @@
(@@) :: Ord a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

fp :: Ord a => (a -> a) -> a -> a
fp f = until (\ x -> x == f x) f

transitiveClosure :: Ord a => Rel a -> Rel a
transitiveClosure = fp (\ s -> (sort.nub) (s ++ (s @@ s)))

isReflexive :: Ord a => Rel a -> Bool
isReflexive r = all (\(x,y) -> (x,x) `elem` r && (y,y) `elem` r) r

isIrreflexive :: Ord a => Rel a -> Bool
isIrreflexive r = all (\(x,y) -> (x,x) `notElem` r && (y,y) `notElem` r) r

isAntisymmetric :: Ord a => Rel a -> Bool
isAntisymmetric r = all (\(x,y) -> x == y || (y,x) `notElem` r) r

isAssymetric :: Ord a => Rel a -> Bool
isAssymetric r = isIrreflexive r && isAntisymmetric r

isSymmetric :: Ord a => Rel a -> Bool
isSymmetric r = containedIn (inverse r) r

isTransitive :: Ord a => Rel a -> Bool
isTransitive r = containedIn (r @@ r) r

isLinear :: Ord a => Rel a -> Bool
isLinear r = all (\(x,y) -> x == y || (x,y) `elem` r || (y,x) `elem` r)  ss
  where
    s = relationToList r
    ss = nub [(x,y) | x <- s, y <- s]

containedIn :: Ord a => [a] -> [a] -> Bool
containedIn xs ys = all (\ x -> x `elem` ys) xs

relationToList :: Ord a => Rel a -> [a]
relationToList [] = []
relationToList ((x,y):ps) = sort (nub s)
  where s = x : y : relationToList ps

relationProperties :: Ord a => [(String, Rel a -> Bool)]
relationProperties = [
                        ("irreflexive", isIrreflexive),
                        ("reflexive", isReflexive),
                        ("assymetric", isAssymetric),
                        ("antiSymmetric", isAntisymmetric),
                        ("symmetric", isSymmetric),
                        ("transitive", isTransitive),
                        ("linear", isLinear)
                    ]

getRelationProperties :: Ord a => Rel a -> [String]
getRelationProperties r = filter (/= "") l
  where l = map (\(n,f) -> if f r then n else "") relationProperties

makeRelation :: Ord a => [a] -> (a -> a -> Bool) -> Rel a
makeRelation ns f = [(x,y) | x <- ns, y <- ns, f x y]

largerThanRelation :: Rel Int
largerThanRelation = makeRelation [-10..10] (<)

largerThanProperties = getRelationProperties (makeRelation [-10..10] (\x y -> x > y))

