import LTS
import AfterImpl
import OutImpl 
import Test.QuickCheck
import System.Random
import qualified Test.FitSpec as FitSpec

fruitImpl :: LTS
fruitImpl = ([1..5], ["?btn", "!apple", "!kiwi"], [(1, "?btn", 2), (2, "!apple", 3), (1, "?btn", 4), (4, "!kiwi", 5)], 1)
fruitModel :: LTS
fruitModel = ([1..4], ["?btn", "!apple"], [(1, "?btn", 2), (2, tau, 3), (2, "!apple", 4)], 1)



ourContains:: Eq a => [a] -> a -> Bool
ourContains [] _ = False
ourContains (x:xs) elemToCheck
    | x == elemToCheck = True
    | otherwise = ourContains xs elemToCheck


-- Will coffee be dispense or not. Basically you are looking for the out function.
-- type IOLTS = ([State], [Label], [Label], [LabeledTransition], State)

traceResult :: Trace -> [Label]
traceResult traceToCheck =
             let fruitImpl = [(1, "?btn", 2), (2, "!apple", 3), (1, "?btn", 4), (4, "!kiwi", 5)]
                 fruitIOLTS = createIOLTS fruitImpl
                 result = out' fruitIOLTS (fruitIOLTS `after'` traceToCheck)
            in result

dispensesFruit :: [Label] -> Bool
dispensesFruit traceToCheck =
        let fruitImpl = [(1, "?btn", 2), (2, "!apple", 3), (1, "?btn", 4), (4, "!kiwi", 5)]
            fruitIOLTS = createIOLTS fruitImpl
            result = out' fruitIOLTS (fruitIOLTS `after'` traceToCheck)
    in
       (ourContains result "apple") || (ourContains result "kiwi")


-- A relation R is coreflexive on a domain A if x, y ∈ A xRy, then necessarily x = y. A relation R is
-- reflexive on a domain A if xRx for every x ∈ A

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getRandomInt' n = getStdRandom (randomR (-n,n))

-- Randomization of the input for the QuickCheck test
genPositiveIntegers:: Gen Integer
genPositiveIntegers = abs <$> (arbitrary :: Gen Integer) `suchThat` (>0)

fib :: Integer -> Integer
fib n = f n 1 1 where
f 0 n1 n2 = n1
f n n1 n2 = f (n-1) n2 (n1+n2) 

prop_nonDecreasingMonotonic :: (Ord a, Num t) => (t -> a) -> t -> Bool
prop_nonDecreasingMonotonic func arg = func arg <= func (arg+1)

prop_increasingMonotonic :: (Num t, Ord t, Ord a) => (t -> a) -> t -> Property
prop_increasingMonotonic func arg = arg > 1 ==> func arg < func (arg+1)

prop_SumOf2Preceeding func arg =  arg > 1 ==>  func arg == func (arg - 1) + func (arg - 2)


properties fib =
  [ property $ prop_nonDecreasingMonotonic fib
  , property $ prop_increasingMonotonic fib
  , property $ prop_SumOf2Preceeding fib
  ]

testFitSpec = mainWith args { names = ["fib n"]
                     , nMutants = 4000
                     , nTests = 4000
                     , timeout = 0
                     }
                (fib :: Integer -> Integer)
                properties


main = do
        quickCheckResult $ forAll genPositiveIntegers (prop_increasingMonotonic fib)
        quickCheckResult $ forAll genPositiveIntegers (prop_nonDecreasingMonotonic fib)
        quickCheckResult $ forAll genPositiveIntegers (prop_SumOf2Preceeding fib)

