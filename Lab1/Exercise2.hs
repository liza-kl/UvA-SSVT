import Data.List
import System.Random
import Test.QuickCheck

subsequences :: [a] -> [[a]]
subsequences [] = [[]]
