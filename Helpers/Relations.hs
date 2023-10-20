module Relations where

import Data.List
-- import Math

type Rel a = [(a,a)]

inverse :: Rel a -> Rel a
inverse = map (\ (x,y) -> (y,x))

unionR :: Rel a -> Rel a -> Rel a
unionR r1 r2 = r1 ++ r2

reflexiveClosure :: Eq a => Rel a -> Rel a
reflexiveClosure r = r ++ [(x, x) | x <- nub (concatMap (\(a, b) -> [a, b]) r)]

symmetricClosure :: Ord a => Rel a -> Rel a
symmetricClosure r = sort ( nub (r ++ inverse r) )

-- Relation composition
infixr 5 @@
(@@) :: Ord a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

fp :: Ord a => (a -> a) -> a -> a
fp f = until (\ x -> x == f x) f

transitiveClosure :: Ord a => Rel a -> Rel a
transitiveClosure r = fp (\ s -> (sort.nub) (s ++ (s @@ s))) r

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
    s = relationToSet r
    ss = nub [(x,y) | x <- s, y <- s]

containedIn :: Ord a => [a] -> [a] -> Bool
containedIn xs ys = all (\ x -> x `elem` ys) xs

relationToSet :: Ord a => Rel a -> [a]
relationToSet [] = []
relationToSet ((x,y):ps) = sort (nub s)
  where s = x : y : relationToSet ps

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

smallestEquivalence :: Ord a => Rel a -> Rel a -> Rel a
smallestEquivalence rel1 rel2 = transitiveClosure (symmetricClosure (unionR (reflexiveClosure rel1) (reflexiveClosure rel2)))

diagonalRelation :: Ord a => [a] -> Rel a
diagonalRelation = map (\ x -> (x, x))