module Denis.Exercise4 where
import Test.QuickCheck
import Data.List
import System.Random
import SetOrd


type Rel a = [(a,a)]

isSerial:: (Eq a, Ord a) => [a] -> Rel a -> Bool
isSerial domain [] = True
isSerial domain (x:xs) = (inSet (fst x) domainSet && inSet (snd x) domainSet) && isSerial domain xs 
                where domainSet = list2set domain