
module Lecture2

where 

import System.Random
import Test.QuickCheck

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getRandomInt' n = getStdRandom (randomR (-n,n))

randomFlip :: Int -> IO Int
randomFlip x = do 
   b <- getRandomInt 1
   if b==0 then return x else return (-x)

genIntList :: IO [Int]
genIntList = do 
  k <- getRandomInt 20
  n <- getRandomInt 10
  getIntL k n

getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do 
   x <-  getRandomInt k
   y <- randomFlip x
   xs <- getIntL k (n-1)
   return (y:xs)

genIntList' :: IO [Int]
genIntList' = do
  k <- getRandomInt 20
  n <- getRandomInt 10
  getIntL' k n

getIntL' :: Int -> Int -> IO [Int]
getIntL' _ 0 = return []
getIntL' k n = do
   x <-  getRandomInt' k
   xs <- getIntL' k (n-1)
   return (x:xs)

quicksort :: Ord a => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) = 
   quicksort [ a | a <- xs, a <= x ]  
   ++ [x]
   ++ quicksort [ a | a <- xs, a > x ]

isTrue :: a -> Bool
isTrue _ = True

prop_ordered :: Ord a => [a] -> Bool
prop_ordered [] = True
prop_ordered (x:xs) = all (>= x) xs && prop_ordered xs

testR :: Int -> Int -> ([Int] -> [Int])
                    -> ([Int] -> [Int] -> Bool) -> IO ()
testR k n f r = if k == n then print (show n ++ " tests passed")
                else do
                  xs <- genIntList
                  if r xs (f xs) then
                    do print ("pass on: " ++ show xs)
                       testR (k+1) n f r
                  else error ("failed test on: " ++ show xs)

testPost :: ([Int] -> [Int]) -> ([Int] -> Bool) -> IO ()
testPost f p = testR 1 100 f (\_ -> p)

testR' :: Int -> Int -> ([Int] -> [Int])
                    -> ([Int] -> [Int] -> Bool) -> IO ()
testR' k n f r = if k == n then print (show n ++ " tests passed")
                else do
                  xs <- genIntList'
                  if r xs (f xs) then
                    do print ("pass on: " ++ show xs)
                       testR' (k+1) n f r
                  else error ("failed test on: " ++ show xs)

testPost' :: ([Int] -> [Int]) -> ([Int] -> Bool) -> IO ()
testPost' f p = testR' 1 100 f (\_ -> p)

quicksrt :: Ord a => [a] -> [a]  
quicksrt [] = []  
quicksrt (x:xs) = 
   quicksrt [ a | a <- xs, a < x ]  
   ++ [x]
   ++ quicksrt [ a | a <- xs, a > x ]

samelength :: [Int] -> [Int] -> Bool
samelength xs ys = length xs == length ys

testRl :: ([Int] -> [Int]) -> ([Int] -> [Int] -> Bool) -> IO ()
testRl f r = testR 1 100 f r

testRl' :: ([Int] -> [Int]) -> ([Int] -> [Int] -> Bool) -> IO ()
testRl' f r = testR' 1 100 f r

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

hoareTest :: (a -> Bool) -> (a -> a) -> (a -> Bool) -> [a] -> Bool
hoareTest precondition f postcondition =
    all (\x -> precondition x --> postcondition (f x))

hoareTestR ::  Fractional t =>
               (a -> Bool)
               -> (a -> a) -> (a -> Bool) -> [a] -> (Bool,t)
hoareTestR precond f postcond testcases = let
       a = fromIntegral (length $ filter precond testcases)
       b = fromIntegral (length testcases)
     in 
       (all (\x ->
         precond x --> postcond (f x)) testcases,a/b)

invarTest :: (a -> Bool) -> (a -> a) -> [a] -> Bool
invarTest invar f = hoareTest invar f invar             

invarTestR ::  Fractional t =>
               (a -> Bool) -> (a -> a) -> [a] -> (Bool,t)
invarTestR invar f = hoareTestR invar f invar             

f1,f2 :: Int -> Int 
f1 = \n -> sum [0..n]
f2 = \n -> (n*(n+1)) `div` 2

test = verboseCheck (\n -> n >= 0 ==> f1 n == f2 n)

parity n = mod n 2    

parity' m n = parity m == parity n

testRel :: (a -> a -> Bool) -> (a -> a) -> [a] -> Bool
testRel spec f = all (\x -> spec x (f x))

testInvar :: Eq b => (a -> b) -> (a -> a) -> [a] -> Bool
testInvar specf = testRel (\ x y -> specf x == specf y)

stronger, weaker :: [a] -> 
       (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 

neg :: (a -> Bool) -> a -> Bool
neg p = \ x -> not (p x)

infixl 2 .&&. 
infixl 2 .||.

(.&&.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p .&&. q = \ x -> p x && q x 

(.||.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p .||. q = \ x -> p x || q x

compar :: [a] -> (a -> Bool) -> (a -> Bool) -> String
compar xs p q = let pq = stronger xs p q 
                    qp = stronger xs q p 
                in 
                  if pq && qp then "equivalent"
                  else if pq  then "stronger"
                  else if qp  then "weaker"
                  else             "incomparable"

infixl 2 #

(#) :: (a -> b) -> (b -> c) -> (a -> c)
(#) = flip (.)

