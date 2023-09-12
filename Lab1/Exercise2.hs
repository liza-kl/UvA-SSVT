import Data.List
import System.Random
import Test.QuickCheck


genSubsequences:: [a] -> [[a]]
genSubsequences [] = [[]]
genSubsequences (x:xs) = genSubsequences xs ++ map (x :) (genSubsequences xs)

checkBaseDef:: Integer -> Bool
checkBaseDef n = length( genSubsequences [1..n]) == 2^n

-- Randomization of the input for the QuickCheck test
genPositiveIntegersOverOne:: Gen Integer
genPositiveIntegersOverOne = abs <$> (arbitrary :: Gen Integer) `suchThat` (>1)

-- Output with QuickCheck tests
main :: IO Result
main = do
        quickCheckResult $ forAll genPositiveIntegersOverOne checkBaseDef

-- TODO WEITER AUSFÜHREN... U GOT DAT... :)
-- Ist wegen 2er Potenzen und Rekursion absolut kacke. Deswegen teste ich da nur part of that specification, weil mein Rechner sonst bumm macht...

