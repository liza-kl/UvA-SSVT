 
import Data.Char

-- Specification 
-- rot 13 replaces the current letter with the 13th letter aftwards
-- if the alphabet ends, the residual places are taken from the beginning

rot13 :: [Char] -> [Char]
getThe13thChar :: Char -> Char 

lowerCaseLetters = ["a" .. "z"]
upperCaseLetters = ["A" .. "Z"]

-- get Index of Element
-- Move from this index by 13 chars
-- get it from the beginning

getThe13thChar char 
    | isUpper char = -- still missing
    | isLower char =  -- still missing
    | otherwise = char
    

rot13 charList = map (\x -> getThe13thChar x ) charList 