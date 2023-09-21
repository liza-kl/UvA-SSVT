module Exercise2 where

import qualified Data.List (subsequences)

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

subsequences:: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = subsequences xs ++ map (x :) (subsequences xs)

-- Checks if the amount of subsequences aligns with the proven amount of subsequences by induction. (Amount of subsequences = 2^n)
prop_checkBaseDef:: Integer -> Property
prop_checkBaseDef n = property (length( subsequences [1..n]) == 2^n)

-- Randomization of the input for the QuickCheck test
-- It is given that the smallest input possible is one since the list has the property for integer lists of the form [1..n].
-- That means that zero won't be considered inside this QuickCheck test even though it should be valid (see induction proof).
genPositiveIntegers:: Gen Integer
genPositiveIntegers = abs <$> (arbitrary :: Gen Integer) `suchThat` (>0)

-- Output with QuickCheck test, which looks if the amount of generated subsequences aligns with the formula proven by induction.
main :: IO Result
main = do
        quickCheckResult $ forAll genPositiveIntegers prop_checkBaseDef

{--

        1. The property is computationally hard to test due to an exponentially rising amount of subsequences (exponential time complexity
        and the domain is +∞).

        2. So we have a high amount of subsequence elements even for smaller lists.
        Since n can be any positive integer larger than 1 we naturally with ever increasing subsequence sizes, the computer reaches a processing limit.
        Even if this limitation wouldn't exist then we would run into an overflow issue due to the corresponding data types.

        I am not really testing a mathematical fact but rather just a small set where we can actually calculate the subsequences for.
        We can't be completely sure using this method that this is completely correct, just the part of the specification which we could account for.
        
        If we were to let this run infinitely for all positive integer values, which would take an infinite amount of time then the check will be sufficient though.

--}