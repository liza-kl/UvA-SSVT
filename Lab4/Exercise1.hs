module Exercise1 where
import Test.QuickCheck
import Data.List
import System.Random
import SetOrd

-- Time estimate 30 mins. (Thanks syntax errors... 🙃)
-- + 20 Mins to refresh on my RNG knowledge. :D 
-- + 20 Mins to write everything up. 😅
-- https://en.wikipedia.org/wiki/Linear_congruential_generator#Sample_code

{-

    The general approach of this exercise is the following...

    Since our goal is to create randomized sets with integers, we start out by generating randomized integer lists.
    We just use a RNG to get a random list length (n) and based on this create a list with randomized integers of length n using the same RNG.
    With this list we can convert it using the pre-given list2set function.
    Duplicate elements from the original integer lists will be lost, but since the sets should be randomized anyways this doesn't matter.
    With this we then have randomized sets generated and put that into a function with the Gen (Set Int) return type and we're done.

-}

-- Random number generation...

{-

    To create those random integer lists/sets we have to have a RNG present.

    For this there are multiple possibilities.
    First the "randomRIO" from System.Random.
    We didn't choose that one since that would introduce the IO monad to everywhere.
    It feels a bit clunky to have that one around needlessly and I want to preserve the "Gen (Set Int)" typing, if possible.
    But this is a totally working method and would work something like this...

    genIOInt:: Int -> Int -> Gen (IO Int)
    genIOInt lowerBound upperBound = do
        return $ randomRIO (lowerBound, upperBound) >>= (\x -> return x)

    Another method would be to use either QuickCheck's "arbitrary" or "choose" function.
    This doesn't technically fulfil the requirement for a "from scratch" implementation.
    To keep the typing of all functions more simplified, we used that version just for the RNG.

    If we were to go REALLY "from scratch", we would need to implement a RNG from scratch.
    We included a version of a linear congruent generator with a fixed seed. 
    (Just due to simplicity of the algorithm.) 
    This version has its flaws though, since its pseudo-RNG can be "easily" detected and structuralized.
    If we were to go completely overboard and have better-randomized lists, 
    one could implement something like a "Mersenne Twister" PRNG algorithm, which is currently used in different programming languages as the standard RNG.

    Info: https://en.wikipedia.org/wiki/Mersenne_Twister

    The desire to do this though is very low, and I feel like I went completely overkill on this exercise already.
    So... No thanks. 🙃

    Make out of that whatever you want, but we have those three possibilities to create random numbers for our intended purpose.

-}

-- QuickCheck RNG
randomInt:: Gen Int
randomInt = arbitrary

-- RNG using a linear congruent generator.

{-

    Used the C++11 modulus (2^31 − 1) and multiplier (48271).
    Other ones would be possible as well...
    Chose that for no particular reason. (Just because it was something previously used by a major programming language.)
    See: https://en.wikipedia.org/wiki/Linear_congruential_generator#Parameters_in_common_use 

    General formula:

    nextSeed = (seed * multiplier) % modulus 

    Right now a fixed seed "1256785" is used.
    This is due to that RNG just being present to show off the general concept.
    One could use the current system time in millis for the seed (-generation).
    This would introduce the IO monad to everything again, which I, at least, didn't want to do.

    This would be something like this...

    currentTime <- getCurrentTime
    let seed = round (utctDayTime currentTime)

    But for now a fixed seed was chosen for RNG.

-}
randomLCGInt :: Gen Int
randomLCGInt = do
    -- nextSeed = (seed * multiplier) % modulus 
    let nextSeed = (1256785 * 48271) `rem` 2147483647
    return nextSeed

-- "From scratch" version.

{-

    Using whatever RNG we can then use it to get a randomized size and the randomized list content.
    We do that as per the following recursive way, to generate our randomized list. 
    We have the size parameter here, just to be more flexible.
    But this is provided by another random number.

-}
generateRandomList:: Int -> Gen [Int]
generateRandomList 0 = pure []
generateRandomList size = do
            randInt <- randomInt
            rest <- generateRandomList (size - 1)
            return (randInt : rest)

-- We can then use the randomized list and the given "list2set" function to create a randomized set from a random list.
generateRandomSet:: Int -> Gen (Set Int)
generateRandomSet size = do
                randomList <- generateRandomList size
                return (list2set randomList)

-- And finally generate a randomized set with a randomized length.
-- Note that both generators are specifically written in a way so infinite sets are possible.
-- So if we were to use those generators in later exercises we need to impose a size limitation on those generators, 
-- due to the necessity of processing those potentially very very very large sets.
-- Since with this restriction the generated sets wouldn't be truly "random", we decided to leave that aspect out in this exercise. 
generateSets :: Gen (Set Int)
generateSets = do
            randInt <- randomInt
            generateRandomSet randInt


-- QuickCheck variant

-- Just use the given arbitrary function from QuickCheck for [Int] to generate random integer lists.
generateRandomList':: Gen [Int]
generateRandomList' = arbitrary

-- And then we can convert those randomized lists using "list2set" to randomized sets.
generateSets':: Gen (Set Int)
generateSets' = do
                list2set <$> generateRandomList'

main :: IO ()
main = do
    fromScratchValue <- generate generateSets
    quickCheckValue <- generate generateSets'

    -- Way to generate limited length values... 
    -- (In principle. Length function for sets implemented in later exercises)
    -- value = suchThat generateSets (\s -> (length s) < 8)

    print fromScratchValue
    print quickCheckValue