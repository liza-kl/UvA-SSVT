module MultiplicationTable where
import Test.QuickCheck
import Data.List

multiplicationTable :: Integer -> [Integer]
multiplicationTable x = map (*x) [1..10]

-- Property 1: Output list has exactly 10 elements
prop_tenElements :: [Integer] -> Integer -> Bool
prop_tenElements o i = length o == 10

-- Property 2: First number is the input
prop_firstElementIsInput :: [Integer] -> Integer -> Bool
prop_firstElementIsInput o i = head o == i

-- Property 3: The sum of the output is the input times the 10th triangle number
prop_sumIsTriangleNumberTimesInput :: [Integer] -> Integer -> Bool
prop_sumIsTriangleNumberTimesInput o i = sum o == sum [1..10] * i

-- Property 4: The difference between consecutive elements is the input
prop_linear :: [Integer] -> Integer -> Bool
prop_linear (x:y:xs) z = y - x == z && prop_linear (y:xs) z
prop_linear _ _ = True

-- Property 5: Any element modulo the input is zero
prop_moduloIsZero :: [Integer] -> Integer -> Bool
prop_moduloIsZero o i = i /= 0 --> all (\v -> v `mod` i == 0) o

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

multiplicationTableProps :: [[Integer] -> Integer -> Bool]
multiplicationTableProps = [prop_tenElements, prop_firstElementIsInput, prop_sumIsTriangleNumberTimesInput, prop_linear, prop_moduloIsZero]
