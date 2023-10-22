module GeneratorsExam where 

import Test.QuickCheck
import GeneratorsLecture
{--
This modules contains every kind of generators that we used during our labs.
Some other stuff to consider if stuck 
https://www.cse.chalmers.se/edu/year/2018/course/TDA452_Functional_Programming/lectures/TestDataGenerators.html
Generator in the Terminal:

sample' <generatorName> -- returns a list of values
generate <generatorName> -- returns one value of the type

Other Generators are in Lecture2.hs
--}

-- Generator without QuickCheck for ints


-- Random QuickCheck integer
genInteger:: Gen Integer
genInteger =  arbitrary 

-- Randomization of the input for the QuickCheck test
genPositiveIntegers:: Gen Integer
genPositiveIntegers = abs <$> (arbitrary :: Gen Integer) `suchThat` (>0)


