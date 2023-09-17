module Exercise3 where

{-- TODO --}
{-# LANGUAGE DerivingVia #-}

import Data.List
import Data.Char
import Data.Maybe
import Test.QuickCheck
import Data.Ord (comparing)
import Debug.Trace
import Text.Printf
import GHC (Name)

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = not p || q
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

data NameableFunction = NameableFunction {
    name:: String,
    propertyFunc:: Int -> Bool
}

one, two, three, four, five :: (Int -> Bool)
one x = even x && x > 3
two x = even x || x > 3
three x = (even x && x > 3) || even x
four x = (even x && x > 3) || even x
five = even

funcsToSort :: [NameableFunction]
funcsToSort = [
    (NameableFunction {name = "one", propertyFunc = one}),
    (NameableFunction {name = "two", propertyFunc = two}),
    (NameableFunction {name = "two", propertyFunc = three}),
    (NameableFunction {name = "four", propertyFunc = four}),
    (NameableFunction {name = "five", propertyFunc = five})]

compareByDomain :: NameableFunction -> NameableFunction -> Ordering
compareByDomain condition1 condition2
    | stronger [-10..10] (getFunc condition1 ) (getFunc condition2) =  GT
    | weaker [-10..10] (getFunc condition1 ) (getFunc condition2) =  LT
    | otherwise = EQ

getName :: NameableFunction -> String
getName (NameableFunction s _) = s

getFunc:: NameableFunction -> (Int -> Bool)
getFunc (NameableFunction _ func) = func

sortedConditions :: [NameableFunction]
sortedConditions = sortBy compareByDomain funcsToSort

main = print [getName x | x <- sortedConditions]