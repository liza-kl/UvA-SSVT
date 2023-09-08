import Data.List
import System.IO

{- 
  time spent: ~ 30 minutes
  not the most efficient way to solve this, but it works
-}

-- we created a function for each condition
pythagoreanTriplet :: Integer -> Integer -> Integer -> Bool
pythagoreanTriplet x y z = x^2 + y^2 == z^2

sumEqualsTo1000 :: Integer -> Integer -> Integer -> Bool
sumEqualsTo1000 x y z = x + y + z == 1000

xSmallerThanYSmallerThanZ :: Integer -> Integer -> Integer -> Bool
xSmallerThanYSmallerThanZ x y z = 0 < x && x < y && y < z && z < 1000

findProduct :: Integer -> Integer -> Integer -> Integer
findProduct x y z = x * y * z

productOfTriplet :: Integer
productOfTriplet = do
    head [findProduct x y z | x <- [0..1000],
                 y <- [x + 1..1000],
                 z <- [y + 1..1000],
                 pythagoreanTriplet x y z,
                 sumEqualsTo1000 x y z,
                 xSmallerThanYSmallerThanZ x y z]


main :: IO ()
main = do
    let productValue = productOfTriplet
    putStrLn (show productValue)