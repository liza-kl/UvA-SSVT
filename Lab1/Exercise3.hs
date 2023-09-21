{-- Time Spent: 90 Minutes, figuring out a workaround how to print it in Haskell --}
module Exercise3 where

import Data.List
import Data.Ord (comparing)

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = not p || q
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

{-- 
Writing a custom data type in order to print the function names more easily.
--}
data NameableFunction = NameableFunction {
    name:: String,
    propertyFunc:: Int -> Bool
}
{-- Functions to access the properties from the NameableFunction type 
(these are going to be used to print the sorting into the command line) 
--}
getName :: NameableFunction -> String
getName (NameableFunction s _) = s

getFunc:: NameableFunction -> (Int -> Bool)
getFunc (NameableFunction _ func) = func

{-- Exercise 3.1
Firstly, I implemented the properties according to the given type.
Also using "even", because it is also compared with the others.

Although "three" and "four" have the same implementation, it is still relevant to include them,
because the computer does not know, that they are equal.
--}
one, two, three, four, five :: (Int -> Bool)
one x = even x && x > 3
two x = even x || x > 3
three x = (even x && x > 3) || even x
four x =  (even x && x > 3)|| even x
five = even

{-- 
Using the implemented properties, also using "even",
--}
funcsToSort :: [NameableFunction]
funcsToSort = [
    (NameableFunction {name = "one", propertyFunc = one}),
    (NameableFunction {name = "two", propertyFunc = two}),
    (NameableFunction {name = "three", propertyFunc = three}),
    (NameableFunction {name = "four", propertyFunc = four}),
    (NameableFunction {name = "five", propertyFunc = five})
    ]

{-- 
The chosen domain has been taken from the exercise descrption. 
--}
compareByDomain :: NameableFunction -> NameableFunction -> Ordering
compareByDomain condition1 condition2
    | stronger [-10..10] (getFunc condition1 ) (getFunc condition2) =  GT
    | weaker [-10..10] (getFunc condition1 ) (getFunc condition2) =  LT
    | otherwise = EQ

{-- Exercise 3.2 --}

{-- Strength in descending order for a domain from [(-10)..10] --}
{--1. "one" – has the smallest set for which the function returns true [4,6,8,10], therefore it is the strongest (most specific one)
    2. "three" - [-10,-8,-6,-4,-2,0,2,4,6,8,10]
    3. "four"  - [-10,-8,-6,-4,-2,0,2,4,6,8,10] 
    4. "five" [-10,-8,-6,-4,-2,0,2,4,6,8,10] three, four, five  are equivalent in strength, because the cardinality is the same.
    5. "two"  - is the weakest property because it applies to the largest set of the domain  [-10,-8,-6,-4,-2,0,2,4,5,6,7,8,10] --}

{-- Initial the sort function sorts in ascending order, therefore the flip to reverse the sort --}

sortedDescConditions :: [NameableFunction]
sortedDescConditions = sortBy (flip compareByDomain) funcsToSort 

main:: IO ()
main = print [getName x | x <- sortedDescConditions]