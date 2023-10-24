module MutationTesting where
    
import qualified Test.QuickCheck as QuickCheck
import Test.FitSpec 

corona1 :: Int -> Int -> Int -> Int -> Int
corona1 r s x0 t = last.init $ scanl (\a b -> s+r*a) x0 [0..t]

-- Example Props.
prop_adheresEquation1 :: (Int,Int,Int,Int) -> Bool
prop_adheresEquation1 (r,s,x0,t) = r^t * x0 + s * ((1 - r^t) `div` (1 - r)) == corona1 r s x0 t

-- Identity Property: For any values of r, s, x0, and t, the result of the corona1 function should be equal to x0 when t is 0.
prop_Identity :: Int -> Int -> Int ->  QuickCheck.Property
prop_Identity r s x0 =  QuickCheck.forAll ( QuickCheck.choose (0, 100)) $ \t ->
  corona1 r s x0 t == x0

-- Monotonicity Property: The result should be non-decreasing as t increases.
prop_Monotonic :: Int -> Int -> Int ->  QuickCheck.Property
prop_Monotonic r s x0 =  QuickCheck.forAll ( QuickCheck.choose (0, 100)) $ \t1 t2 ->
  t1 <= t2 ==> corona1 r s x0 t1 <= corona1 r s x0 t2

-- Consistency Property: If t is 0, the result should be equal to x0. Otherwise, it should not be equal to x0.
prop_Consistency :: Int -> Int -> Int ->  QuickCheck.Property
prop_Consistency r s x0 =  QuickCheck.forAll ( QuickCheck.choose (1, 100)) $ \t ->
  (corona1 r s x0 t == x0) == (t == 0)



-- First make sure to test the function based on QuickCheck.
-- Explain, why you used certain properties.
-- A bulletproof property is going with the equation or add an edge case such as 0.
-- Then check your properties with mutation testing. So you are testing your tests.


-- This is needed, because QuickCheck is not going to generate like 4 numbers for you. 
quickCheckCorona::   QuickCheck.Gen (Int, Int, Int, Int)
quickCheckCorona = do
                          g1 <-   QuickCheck.choose(1,100)
                          g2 <-   QuickCheck.choose(1,100)
                          r1 <-   QuickCheck.choose(1,100)
                          r2 <-   QuickCheck.choose(1,100)
                          return (g1,g2, r1, r2)

quickCheckTriple::   QuickCheck.Gen (Int, Int, Int)
quickCheckTriple = do
                          g1 <-   QuickCheck.choose(1,100)
                          g2 <-   QuickCheck.choose(1,100)
                          r1 <-   QuickCheck.choose(1,100)
                          return (g1,g2, r1)


-- With this equation fitspec is going to get stuck. 
-- prop: Xt+1 - Xt = (R - 1) * Xt + S
prop_recurrenceEquation :: (Int -> Int -> Int -> Int -> Int) -> Int -> Int -> Int -> Int -> Bool
prop_recurrenceEquation func r s x0 t = (t >= 1 &&t < 2 &&  r >= 1 && r < 2 && s >= 1 && s < 2 &&  x0 >= 1 && x0 < 2) ==> func r s x0 (t+1) - func r s x0 t == (r-1) * func r s x0 t + s

-- Bring the X_t only on one side and inject only positive numbers, then it's working. 
prop_rewrittenEquation :: (Int -> Int -> Int -> Int -> Int) -> Int -> Int -> Int -> Int -> Bool
prop_rewrittenEquation  func r s x0 t =  (t >= 1  &&  r >= 1  && s >= 1  &&  x0 >= 1 ) ==> (func r s x0 (t+1)) == r * (func r s x0 t) - s 

main = 
    do 
      QuickCheck.quickCheckResult $  QuickCheck.forAll quickCheckCorona prop_adheresEquation1
      QuickCheck.verboseCheckResult $  prop_Identity
      QuickCheck.verboseCheckResult $  prop_Consistency
      QuickCheck.verboseCheckResult $ prop_Monotonic
      
properties corona1 =
  [ property $ prop_rewrittenEquation corona1
  ]

testFitSpec = reportWith args { names = ["corona1"]
                     , nMutants = 10
                     , nTests = 10
                     , timeout = 5
                     }
                (corona1 :: Int -> Int -> Int -> Int -> Int)
                properties

