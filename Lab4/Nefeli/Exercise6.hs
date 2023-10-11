module Exercise6 where
import Test.QuickCheck
import Data.List
import System.Random
import SetOrd
import Exercise5

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

prop_elemInRelationIsInTransitiveClosure :: Eq a => Gen (Rel a) -> Bool
prop_elemInRelationIsInTransitiveClosure relation = 
    let transitiveClosure = trClos relation
    in all (\x -> x `elem` transitiveClosure) relation

main :: IO ()
main = do
    -- let relation = [(1, 2), (2, 3), (3, 4)]
    -- let transitiveClosure = trClos relation
    -- print transitiveClosure
    quickCheck prop_elemInRelationIsInTransitiveClosure