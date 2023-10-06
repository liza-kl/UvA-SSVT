module Exercise1 where
import Data.List
import Test.QuickCheck
import Mutation
import System.Random
import MultiplicationTable (multiplicationTable)

-- ## Deliverables ##
-- List of mutators and rationale, implementation, indication of time spent.
-- Time spent: 60 Minutes
-- Note: Input for fut is for the sake of ease through all of the exercises always
-- 5. No special meaning.

-- ## List of Mutators and Rationale ##

-- Weak Mutant in our understanding: A mutant which gets caught by as many properties as possible.
-- Strong Mutant in our understanding: A mutant which gets caught by as few properties as possible.

    -- We want to check:
    -- - edge cases like empty lists
    -- - list length changes 
    -- - element value changes
    -- - patterns e.g. sort, reverse, shuffle, same-number list

    -- Outputs that are covered:
    -- 1) Add elements to start or end of list -> 
    --     - Strength: Increases list length.
    --     - Weakness: Only adds in the start or end of the list. Does not remove elements or check edge cases/patterns.
    -- 2) Remove random elements -> 
    --     - Strength: Decreases the list length. 
    --     - Weakness: Does not add elements or check edge cases/patterns.
    -- 3) Random list ->
    --     - Strength: Checks what happens with different data.
    --     - Weakness: Does not change the existing list.

    -- Outputs that are not covered:
    -- 1) Empty output list -> 
    --     - Strength: Checks the edge case of empty list.
    --     - Weakness: Does not add or remove or modify elements. Does not check patterns.
    -- 2) Sort output list ->
    --     - Strength: Checks what happens with a sorted list.
    --     - Weakness: Does not add or remove or modify elements.
    -- 3) Reverse output list -> 
    --     - Strength: Checks what happens with the reversed list.
    --     - Weakness: Does not add or remove or modify elements.
    -- 4) Shuffle output list -> 
    --     - Strength: Checks what happens if the elements are in different positions.
    --     - Weakness: Does not add or remove or modify elements.
    -- 5) Negate output list ->
    --     - Strength: Checks what happens if the numbers are negative.
    --     - Weakness: Does not add or remove or modify elements.
    -- 6) Replace random values in output list ->
    --     - Strength: Checks what happens if some values are changed.
    --     - Weakness: Does not add or remove elements. Does not check patterns.
    -- 7) Output list that contains the same number ->
    --     - Strength: Checks the edge case of having duplicates.
    --     - Weakness: Does not add or remove elements. 
    -- 8) Add numbers in random spots or in specific positions, like the first, last, middle element. 
    --     - Strength: Random numbers in "random" spots. Increases list length.
    --     - Weakness: Does not remove elements.  Does not check patterns.
    -- 9) An exact same list which could arise because of the arbitrary list generator
    --     - This would be a so-called "equivalent mutant" which unfortunately can't be really tested.
    -- 10) Replacing the output with a given, fixed list. 
    --     - Strength: Look for specialized edge cases (for example a list of doubles in this case) and it is super fast.
    --                 For example you can check for edge cases in length or difference dependent properties.
    --     - Weakness: Very limited to highly-specific outputs.
    -- 11) Applying any mathematical operation to every element of a list.
    --     For example multiplying every number by a given factor, or taking the n-th root or applying the n-th power of every value.
    --     - Strength: -
    --     - Weakness: The "ratio" between the differences of each element stays the same. So the general pattern of the output list remains.
    -- 12) Applying mathematical operations of one list with n other lists.
    --     For example adding [2,3,4] and [5,2,1] together to [7,5,5] or multiplying [2,1,0] and [1,2,3] to [2,2,0].
    --     - Strength: Same as in 11) but resulting list is more "randomized".
    --     - Weakness: Does not add or remove elements. Does not check patterns.

    -- Because we are limited to modifying the output of the modified function and not the actual source code, we can't use "real mutators".
    -- Therefore we are also limited to a small set of the real possibilities in this case.
    -- For example introducing runtime errors (invalid mutant) is way more limited and could only be done by applying an illegal operation to the corresponding output list.

-- ## Implementation ##

emptyList :: [Integer] -> Gen [Integer]
emptyList xs = return []

sortList :: [Integer] -> Gen [Integer]
sortList xs = return (sort xs)

reverseList :: [Integer] -> Gen [Integer]
reverseList xs = return (reverse xs)

shuffleList :: [Integer] -> Gen [Integer]
shuffleList = shuffle

negativeList :: [Integer] -> Gen [Integer]
negativeList xs = return (map negate xs)

replaceAtIndex :: Int -> Integer -> [Integer] -> [Integer]
replaceAtIndex index newValue xs =
  take index xs ++ [newValue] ++ drop (index + 1) xs

-- ## HelperFunctions ##
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
    let nums = multiplicationTable 5

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