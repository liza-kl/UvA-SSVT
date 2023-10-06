module Exercise4 where
import Exercise2
import Data.List
import Test.QuickCheck

{-

    The general approach in this exercise is the following:
        - Count up the amount of killed mutants for a given set of properties.
        - Divide that by the amount of overall mutants present to calculate the percentage of killed mutants.
    
    This percentage is an estimator of how strong a given set of properties is. 
    The lower the number is, the weaker the set of properties is, and the higher it is, the stronger the properties are.
    So, for example, a strength percentage of 10% would indicate really weak properties, whereas, say, 80% would indicate a strong set of properties.

    In principle, we could use the "countSurvivors" function from exercise 2 and use the following approach:
        - (Total amount of mutants) - (countSurvivors of the property set) = Amount of killed mutants
        -> (Amount of killed mutants) / (Total amount of mutants) = Percentage of killed mutants.
    
    In this case, it would most likely work since the compromised list of the "mutate'" function (for most "easy/non-complex" mutants) would return a list of just booleans, thus having a clear-cut definition of a killed and survived mutant.
    Assuming this special case, we could say that... (Total amount of mutants) - (countSurvivors of the property set) = Amount of killed mutants.
    Since "mutate'" could also contain blank elements, though, this approach isn't quite accurate since the previous formula would include mutants where the result would be a crash, a timeout or similar events.
    So actually... 
    (Total amount of mutants) - (countSurvivors of the property set) = (Amount of killed mutants) + (Amount of "crashed" (i.e. due to timeouts, crashing, compile errors, etc.) mutants)

    To be extra accurate, we modified the countSurvivors from exercise 2 to account for this special case at the point where it checks if all elements of the "mutantsSurvived" list are true or false to see if all mutants were killed instead of still living.
    Then, we get a function "countKilled" and exclude the "blank" elements.

    The final formula for the percentage results is...
    -> Percentage of killed elements = (countDead of the property set) / (Total amount of mutants)

    The final indicator would have the same valuing and meaning as a strength indicator of that property set, though.

    With this percentage, we can easily check the strength of certain properties,
    thus being able to assert how "good" your tests/properties are, since stronger tests most of the time (except in cases where you only check for specific values, where this would be kinda bad since it checks for a really limited amount of cases) imply that more gets accounted for and the quality of the test is higher as well.
    (Strong tests don't imply good coverage, but when you have good coverage, generalized tests and strong properties, which you check for, you have higher quality (better) checks, which this percentage (partially) takes into account.)

    TLDR: (since I wrote all of that while being absolutely overcaffeinated, and you probably have something better to do than reading a "shitty" version of half of the lecture content on mutation testing again...)

    Approach:
    ((Total amount of mutants) - (countSurvivors of the property set) = Amount of killed mutants) / (Total amount of mutants) = Percentage of killed mutants.
    
    High percentage -> Strong property set -> Most likely higher quality properties if not too specific and good coverage is implied.
    -> big number = good (kinda... :D)

-}

-- If you wanna be REALLY precise you'd have to filter from all of the false booleans inside that weird ass thing.
-- Since inverse of countSurvivors (From Exercise 2) doesn't take into account any differences between killed mutants and timed out ones, for example.
calculateSurvivorPercentage:: Integer -> [[Integer] -> Integer -> Bool] -> ([Integer] -> Gen [Integer]) -> (Integer -> [Integer]) -> IO Float

-- Approximate approach using countSurvivors
-- calculateSurvivorPercentage numberOfMutants mutators properties func = fromIntegral (numberOfMutants - (countSurvivors numberOfMutants mutators properties func)) / fromIntegral numberOfMutants

-- More "accurate" approach using "countKills".
calculateSurvivorPercentage numberOfMutants props mutators fut = do 
        killedMutants <- (countKills numberOfMutants props mutators fut)
        let result = (fromIntegral killedMutants) / (fromIntegral numberOfMutants)
        return result

-- So we introduce a modified version of countSurvivors from exercise 2 with the only difference being the filter where we filter for True instead of False to get the killed mutants instead of the survived ones.
-- We return an IO Integer by using the "generate" function, instead of an Gen Integer, to make handling in percentage calculation simpler.
-- The rest stays the same and uses the code from exercise 2.
countKills :: Integer -> [[Integer] -> Integer -> Bool] -> ([Integer] -> Gen [Integer]) -> (Integer -> [Integer]) -> IO Integer
countKills numberOfMutants props mutators fut =
    generate (toInteger . length . filter (== True) <$> sequence (generateListOfSurvivedMutants numberOfMutants mutators props fut))