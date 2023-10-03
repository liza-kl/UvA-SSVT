
module Exercise1 where
import Data.List
import Test.QuickCheck
import Mutation
import System.Random

{-- 
    Time Spent: 1 hour

    We want to check:
    - edge cases like empty lists
    - list length changes 
    - element value changes
    - patterns e.g. sort, reverse, shuffle, same-number list

    Outputs that are covered:
    1) Add elements to start or end of list -> 
        - Strength: Increases list length.
        - Weakness: Only adds in the start or end of the list. Does not remove elements or check edge cases/patterns.
    2) Remove random elements -> 
        - Strength: Decreases the list length. 
        - Weakness: Does not add elements or check edge cases/patterns.
    3) Random list ->
        - Strength: Checks what happens with different data.
        - Weakness: Does not change the existing list.

    Outputs that are not covered:
    1) Empty output list -> 
        - Strength: Checks the edge case of empty list.
        - Weakness: Does not add or remove or modify elements. Does not check patterns.
    2) Sort output list ->
        - Strength: Checks what happens with a sorted list.
        - Weakness: Does not add or remove or modify elements.
    3) Reverse output list -> 
        - Strength: Checks what happens with the reversed list.
        - Weakness: Does not add or remove or modify elements.
    4) Shuffle output list -> 
        - Strength: Checks what happens if we the elements are in different positions.
        - Weakness: Does not add or remove or modify elements.
    5) Negate output list ->
        - Strength: Checks what happens if the numbers are negative.
        - Weakness: Does not add or remove or modify elements.
    6) Replace random values in output list ->
        - Strength: Checks what happens if some values are changed.
        - Weakness: Does not add or remove elements. Does not check patterns.
    7) Output list that contains the same number ->
        - Strength: Checks the edge case of having duplicates.
        - Weakness: Does not add or remove elements. 
    8) Add numbers in random spots 
        - Strength: Random numbers in random spots. Increases list length.
        - Weakness: Does not remove elements.  Does not check patterns.
--}

emptyList :: [Integer] -> Gen [Integer]
emptyList xs = return []

sortList :: [Integer] -> Gen [Integer]
sortList xs = return (sort xs)

reverseList :: [Integer] -> Gen [Integer]
reverseList xs = return (reverse xs)

shuffleList :: [Integer] -> Gen [Integer]
shuffleList xs = shuffle xs

negativeList :: [Integer] -> Gen [Integer]
negativeList xs = return (map negate xs)

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex index newValue xs =
  take index xs ++ [newValue] ++ drop (index + 1) xs

-- helper function
changeList :: [Integer] -> Gen [Integer]
changeList xs = do
    indicesToReplace <- sublistOf [0..(length xs - 1)] 
    valuesToReplaceWith <- vectorOf (length indicesToReplace) arbitrary 
    return $ foldl (\list (index, newValue) -> replaceAtIndex index newValue list) xs (zip indicesToReplace valuesToReplaceWith)

sameNumberList :: [Integer] -> Gen [Integer]
sameNumberList xs = do 
    num <- choose (1, 100)
    return (replicate (length xs) num)

addElementsRandomly :: [Integer] -> Gen [Integer]
addElementsRandomly xs = do
    toAdd <- listOf arbitrary
    indicesToAdd <- sublistOf [0..length xs]
    let insertAtIndex index value xs = take index xs ++ [value] ++ drop index xs
    return $ foldr (\(index, value) list -> insertAtIndex index value list) xs (zip indicesToAdd toAdd)

main :: IO ()
main =  do
    -- simple example
    let nums = [1, 2, 3, 4, 5]

    empty <- generate (emptyList nums)
    sorted <- generate (sortList nums)
    reversed <- generate (reverseList nums)
    shuffled <- generate (shuffleList nums)
    negative <- generate (negativeList nums)
    changed <- generate (changeList nums)
    sameNumber <- generate (sameNumberList nums)
    added <- generate (addElements nums)
    addedRandomly <- generate (addElementsRandomly nums)
    removed <- generate (removeElements nums)
    anylist <- generate (anyList nums)
    
    -- Mutated lists
    putStrLn "Empty list:"
    print empty
    putStrLn "\n"

    putStrLn "Sorted list:"
    print sorted
    putStrLn "\n"
    
    putStrLn "Reversed list:"
    print reversed
    putStrLn "\n"
    
    putStrLn "Shuffled list:"
    print shuffled
    putStrLn "\n"

    putStrLn "Negative list:"
    print negative
    putStrLn "\n"

    putStrLn "Modified list:"
    print changed
    putStrLn "\n"

    putStrLn "Same-number list:"
    print sameNumber
    putStrLn "\n"

    putStrLn "Add to list:"
    print added
    putStrLn "\n"

    putStrLn "Add to list randomly:"
    print addedRandomly
    putStrLn "\n"

    putStrLn "Remove from list:"
    print removed
    putStrLn "\n"

    putStrLn "Random list:"
    print anylist
    putStrLn "\n"