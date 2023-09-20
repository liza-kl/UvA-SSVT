module Sets where

import Data.List

{------------------------------------------------------------------------------
                    Definitions

-------------------------------------------------------------------------------}

newtype Set a = Set [a] deriving (Eq,Ord)

instance (Show a) => Show (Set a) where
    showsPrec _ (Set s) str = showSet s str

showSet []     str = showString "{}" str
showSet (x:xs) str = showChar '{' ( shows x ( showl xs str))
     where showl []     str = showChar '}' str
           showl (x:xs) str = showChar ',' (shows x (showl xs str))


list2set :: Ord a => [a] -> Set a
list2set [] = Set []
list2set (x:xs) = insertSet x (list2set xs)

insertSet :: (Ord a) => a -> Set a -> Set a
insertSet x (Set s) = Set (insertList x s)

insertList x [] = [x]
insertList x ys@(y:ys') = case compare x y of
                                 GT -> y : insertList x ys'
                                 EQ -> ys
                                 _  -> x : ys

{------------------------------------------------------------------------------
                   Implementations

-------------------------------------------------------------------------------}

setIntersection :: (Ord a) => Set a -> Set a -> Set a
setIntersection (Set xs) (Set ys) = Set (xs `intersect` ys)

setUnion :: (Ord a) => Set a -> Set a -> Set a
setUnion (Set xs) (Set ys) = list2set (xs ++ ys)

setDiffrence :: (Ord a) => Set a -> Set a -> Set a
setDiffrence (Set xs) (Set ys) = list2set (xs \\ ys)

getSetObjects :: (Ord a) => Set a -> [a]
getSetObjects (Set x) = x