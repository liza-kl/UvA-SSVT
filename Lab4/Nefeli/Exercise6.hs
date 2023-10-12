module Exercise6 where
import Test.QuickCheck
import Data.List
import System.Random
import SetOrd
import Exercise5
import Exercise3

-- random pair
randomPair :: Gen (Int, Int)
randomPair = do
  x <- arbitrary
  y <- arbitrary
  return (x, y)

-- random relation
randomRel :: Int -> Gen (Rel Int)
randomRel size = vectorOf size randomPair

-- arbitrary instance
instance Arbitrary (Rel Int) where
  arbitrary = sized randomRel

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
