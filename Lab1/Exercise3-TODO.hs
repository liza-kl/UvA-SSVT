module Exercise3 where

{-- TODO --}

import Data.List
import Data.Char
import Data.Maybe
import Test.QuickCheck
import Data.Ord (comparing)


infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = not p || q
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p


one, two, three, four, five :: Int -> Bool
one x = even x && x > 3
two x = even x || x > 3
three x = (even x && x > 3) || even x
four x = (even x && x > 3) || even x
five = even

compareByDomain :: (Int -> Bool) -> (Int -> Bool) -> Ordering
compareByDomain condition1 condition2
    | stronger [-10..10] condition1 condition2 = GT
    | stronger [-10..10] condition2 condition1  = LT
    | otherwise = EQ


conditions = [one, two, three, four, five ]
sortedConditions :: [Int -> Bool]
sortedConditions = sortBy compareByDomain conditions

{-- TODO Find out how to print it x) --}