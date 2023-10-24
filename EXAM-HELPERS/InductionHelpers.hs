module InductionHelper where
import Test.QuickCheck

numR 0 = 1
numR n = 2^(2*n-2) * numR (n-1)

-- numR' 0 = 1
numR' n = 2^(n^2-n)


-- Properties to consider
prop_baseCase :: Bool
prop_baseCase = numR 0 == 1
prop_recursiveCase :: Positive Int -> Bool
prop_recursiveCase (Positive n) = numR n == 2^(2*n-2) * numR (n-1)

prop_monotonicity ::  Int ->  Int -> Property
prop_monotonicity n m = (n >= 0) && (m >= 0) ==> (n <= m) ==> (numR n <= numR m)
prop_factorialRelation ::  Int -> Property
prop_factorialRelation n = n >= 0 ==> numR n == product [1..n]

prop_consistency ::  Int -> Property
prop_consistency n = n >= 0 ==> numR n ==  2^(n^2-n)


arbNumber :: Gen (Positive Int)
arbNumber = arbitrary

main = do
    quickCheckResult $ forAll arbNumber prop_recursiveCase
    verboseCheck prop_baseCase