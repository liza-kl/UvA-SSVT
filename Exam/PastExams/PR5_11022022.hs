module PR5_11022022 where

import Test.QuickCheck 

corona1 :: Int -> Int -> Int -> Int -> Int
corona1 r s x0 t = last.init $ scanl (\a b -> s+r*a) x0 [0..t]
corona2 :: Int -> Int -> Int -> Int -> Int
corona2 r s x0 0 = x0
corona2 r s x0 t = s + r * (corona2 r s x0 $ t-1)
corona3 :: Int -> Int -> Int -> Int -> Int
corona3 r s x0 0 = s
corona3 r s x0 t = iterate ((s+).(r*)) x0 !! t
corona4 :: Int -> Int -> Int -> Int -> Int
corona4 r s x0 t = (!! t) $ zipWith (+) (iterate (+s) 0) (iterate (*r) x0)
corona5 :: Int -> Int -> Int -> Int -> Int
corona5 r s x0 t = r^t*x0 + s * (r^t-1) `div`(r-1)

prop_adheresEquation1 :: (Int,Int,Int,Int) -> Bool
prop_adheresEquation1 (r,s,x0,t) = r^t * x0 + s * ((1 - r^t) `div` (1 - r)) == corona1 r s x0 t

prop_adheresEquation2 :: (Int,Int,Int,Int) -> Bool
prop_adheresEquation2 (r,s,x0,t) = r^t * x0 + s * ((1 - r^t) `div` (1 - r)) == corona2 r s x0 t

prop_adheresEquation3 :: (Int,Int,Int,Int) -> Bool
prop_adheresEquation3 (r,s,x0,t) = r^t * x0 + s * ((1 - r^t) `div` (1 - r)) == corona3 r s x0 t

prop_adheresEquation4 :: (Int,Int,Int,Int) -> Bool
prop_adheresEquation4 (r,s,x0,t) = r^t * x0 + s * ((1 - r^t) `div` (1 - r)) == corona4 r s x0 t

prop_adheresEquation5 :: (Int,Int,Int,Int) -> Bool
prop_adheresEquation5 (r,s,x0,t) = r^t * x0 + s * ((1 - r^t) `div` (1 - r)) == corona5 r s x0 t

prop_otherEquation1::  (Int,Int,Int,Int) -> Bool
prop_otherEquation1 (r,s,x0,t) = corona1 r s x0 (t+1) - corona1 r s x0 (t) == (r-1) *  corona1 r s x0 (t) + s
 
prop_otherEquation2::  (Int,Int,Int,Int) -> Bool
prop_otherEquation2 (r,s,x0,t) = corona2 r s x0 (t+1) - corona2 r s x0 (t) == (r-1) *  corona2 r s x0 (t) + s

prop_otherEquation3::  (Int,Int,Int,Int) -> Bool
prop_otherEquation3 (r,s,x0,t) = corona3 r s x0 (t+1) - corona3 r s x0 (t) == (r-1) *  corona3 r s x0 (t) + s

prop_otherEquation4::  (Int,Int,Int,Int) -> Bool
prop_otherEquation4 (r,s,x0,t) = corona4 r s x0 (t+1) - corona4 r s x0 (t) == (r-1) *  corona4 r s x0 (t) + s

prop_otherEquation5::  (Int,Int,Int,Int) -> Bool
prop_otherEquation5 (r,s,x0,t) = corona4 r s x0 (t+1) - corona4 r s x0 (t) == (r-1) *  corona4 r s x0 (t) + s

quickCheckCorona:: Gen (Int, Int, Int, Int)
quickCheckCorona = do
                          g1 <- choose(1,100)
                          g2 <- choose(1,100)
                          r1 <- choose(1,100)
                          r2 <- choose(1,100)
                          return (g1,g2, r1, r2)

-- Randomization of the input for the QuickCheck test
genPositiveInts:: Gen Int
genPositiveInts = abs <$> (arbitrary :: Gen Int) `suchThat` (>0)

main = 
    do 
        quickCheckResult $ forAll quickCheckCorona prop_otherEquation1
        quickCheckResult $ forAll quickCheckCorona prop_otherEquation2
        quickCheckResult $ forAll quickCheckCorona prop_otherEquation3
        quickCheckResult $ forAll quickCheckCorona prop_otherEquation4
        quickCheckResult $ forAll quickCheckCorona prop_otherEquation5
        -- quickCheckResult $ forAll quickCheckCorona prop_adheresEquation1
        -- quickCheckResult $ forAll quickCheckCorona prop_adheresEquation2
        -- quickCheckResult $ forAll quickCheckCorona prop_adheresEquation3
        -- quickCheckResult $ forAll quickCheckCorona prop_adheresEquation4
        -- quickCheckResult $ forAll quickCheckCorona prop_adheresEquation5

-- properties corona4 =
--   [ property $ prop_adheresEquation 
--   ]

-- testFitSpec = mainWith args { names = ["corona4 r s x0 t "]
--                      , nMutants = 4000
--                      , nTests = 4000
--                      , timeout = 0
--                      }
--                 (corona1 :: Int -> Int -> Int -> Int -> Int)
--                 properties

