module Exercise7 where
import qualified Exercise6 as Ex6
import qualified Exercise3 as Ex3
import qualified Exercise5 as Ex5
import Data.List
import Test.QuickCheck
import SetOrd

    -- Indication of time spent: 20 Minutes

    -- 1.) Example for when the symmetric closure of the transitive closure != transitive closure of the symmetric closure
    -- Relation:[ (1, 2), (2, 3), (3, 4)]
    -- Transitive Closure [(1,2),(2,3),(3,4),(1,3),(2,4),(1,4)]
    -- Symmetric Closure:  [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)]
    -- Symmetric closure of transitive closure: [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3),(1,3),(3,1),(2,4),(4,2),(1,4),(4,1)]
    -- Transitive closure of symmetric closure: [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3),(1,1),(1,3),(2,2),(2,4),(3,1),(3,3),(4,2),(4,4),(1,4),(4,1)]

    -- 1.1.) Why? Because the symmetric closure inverses all the tuples in a set. So, if you create a transitive closure
    -- over this symmetric relation, you will inevitabely run into the case that the "identity" tuples will be added during the 
    -- transitive closure. Transitive closure states that if a ~ b and b ~ c than a ~ c or if (a1, a2) ∈ R and (a2, a3) ∈ R, then (a1, a3) ∈ R. 
    -- So if you have a tuple (1,0) create the inverse (0,1) you will have in the transitive closure the (1,1) tuple.

    -- 2.) Examples for then the symmetric closure of the transitive closure == transitive closure of the symmetric closure
    -- 2.1) If you have an empty set, then the symmetric closure will be an empty set but also the transitive closure.
    -- 2.2) If you have a set of solely identities e.g. [(1,1),(2,2)], then you are going to have the case symmetric closure of the transitive closure == transitive closure of the symmetric closure
    -- because a == b and b == c and a == c ("looping")
    -- 2.3) If you have a relation with only one binary tuple in it, the statement applies [(1,1)], because then you are only referring to yourself
    -- 2.4) If you have an identity tuple and another tuple where the numbers not equal the identity tuple number. e.g. [(-3,-1),(5,5)], because then you have no "c" for the
    -- transitive relation 

    -- Conclusion: Both cases are possible. 
    -- Create a quickcheck test with relation stuff and falsify 

    -- To give more examples we can create QuickCheck and check with verbose. We can use our Generator from Exercise 
    -- 6 for that 

    -- Some other findings: 
    -- I just created some generators to "automate" the process of generating examples 
    -- + I found some other examples for the second case 
    -- - Took me a bit of time

type Rel a = [(a,a)]

getTransSymClos :: Ord a => Rel a -> Rel a
getTransSymClos rel = Ex3.symClos (Ex5.trClos rel)

getSymTransClos :: Ord a => Rel a -> Rel a
getSymTransClos rel = Ex5.trClos (Ex3.symClos rel)

relationToSet :: Ord a => Rel a -> [a]
relationToSet [] = []
relationToSet ((x,y):ps) = sort (nub s)
  where s = x : y : relationToSet ps


compareRels :: Rel Int -> Rel Int -> Bool
compareRels [] [] = True 
compareRels _ [] = False 
compareRels [] _ = False 
compareRels ((a,b):xs) ((c,d):ys) 
    | (a,b) `elem` ((c,d):ys) = compareRels xs ys
    | otherwise = False 


prop_symCloseIstransClose ::Rel Int -> Bool
prop_symCloseIstransClose rel = compareRels (getTransSymClos rel) (getSymTransClos rel)

prop_symCloseNottransClose :: Rel Int -> Bool
prop_symCloseNottransClose rel = not (compareRels (getTransSymClos rel) (getSymTransClos rel))

genSymClosEqTrClosRel :: Gen (Rel Int)
genSymClosEqTrClosRel = do
    Ex6.genRel `suchThat` prop_symCloseIstransClose

genSymClosNEqTrClosRel :: Gen (Rel Int)
genSymClosNEqTrClosRel = do
    Ex6.genRel `suchThat` prop_symCloseNottransClose


main = do
    a <- sample' genSymClosEqTrClosRel
    b <- sample' genSymClosNEqTrClosRel
    print "Generating Examples where the symmetric closure of the transitive closure equals the transitive closure of the symmetric one"
    print a
    print "Generating Examples where the symmetric closure of the transitive closure does **not** equal the transitive closure of the symmetric one"
    print b


