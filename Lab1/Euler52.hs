module Euler52 where

import Data.List

-- To test that this function works as expected I would run property tests
-- Some properties could be:
-- The result should be a positive number and >= 10, so that it makes sense
-- If i run the function with this number as argument, i will again get the same result
-- If i run the function with this number + 1 as argument, i will not get the same result
-- And obviously, i could check that x, 2x, 3x, 4x, 5x, 6x are consisted of the same numbers

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
