module Euler52 where

import Data.List

-- TODO : explain how it could be tested

smallestPermutedMultiples :: Integer -> IO Integer
smallestPermutedMultiples x = do

    let condition = isPermutation x (2*x) && isPermutation x (3*x) && isPermutation x (4*x) && isPermutation x (5*x) && isPermutation x (6*x)
    -- if so, return
    if condition
        then return x
    -- otherwise search again starting from the next number, recursively
        else smallestPermutedMultiples(x + 1)

isPermutation :: Integer -> Integer -> Bool
isPermutation x y =
    sort (show x) == sort (show y)

main :: IO ()
main = do
    smallestPermutedMultiple <- smallestPermutedMultiples 10
    putStrLn (show smallestPermutedMultiple)
