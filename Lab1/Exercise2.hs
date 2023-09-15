module Exercise2 where

import Data.List
import System.Random
import Test.QuickCheck

-- Time Spent ~ 1h20 
-- Due to conceptualizing and Haskell issues...

{- 

        Proof by Induction that if A is finite set with |A| = n then |P(A)| = 2^n

        Base case (n = 0): Empty set has 0 members and P(∅) = {∅} -> 1 Member -> Holds true since 2^0 also equals 1.

        I.H. show for (n+1) -> |P(B)| = 2^(n+1).
        P(B) contains all of the elements of P(A) as well as the (n+1)th element.
        Since we need to unify every pre-existing element with the (n+1)th element the have to double the amount of elements.
        Thus the amount of elements for |P(B)| = 2 * 2^n = 2^(n+1) 
        -> Hypothesis holds true. □

-}

genSubsequences:: [a] -> [[a]]
genSubsequences [] = [[]]
genSubsequences (x:xs) = genSubsequences xs ++ map (x :) (genSubsequences xs)

checkBaseDef:: Integer -> Bool
checkBaseDef n = length( genSubsequences [1..n]) == 2^n

-- Randomization of the input for the QuickCheck test
genPositiveIntegersOverOne:: Gen Integer
genPositiveIntegersOverOne = abs <$> (arbitrary :: Gen Integer) `suchThat` (>1)

-- Output with QuickCheck tests
main :: IO Result
main = do
        quickCheckResult $ forAll genPositiveIntegersOverOne checkBaseDef

{-

        The property is hard to test due to an exponentially rising amount of subsequences.
        So we have a high amount of subsequence elements even for smaller lists.
        Since n can be any positive integer larger than 1 we naturally with ever increasing subsequence sizes, the computer reaches a processing limit.
        Even if this limitation wouldn't exist then we would run into an overflow issue due to the corresponding data types.

        I am not really testing a mathematical fact but rather just a small set where we can actually calculate the subsequences for.
        We can't be completely sure using this method that this is completely correct, just the part of the specification which we could account for.
        
-}