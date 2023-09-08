import Data.Char
import Test.QuickCheck

-- Rot13 Encoding
-- Time needed ~2h

{-
    Specification:
        - A lower case letter should be replaced by the lower case letter which comes 13 letters later in the alphabet. Example "a" -> "n"
        - An upper case letter should be replaced by the upper case letter which comes 13 letters later in the alphabet. Example "A" -> "N"
        - If there isn't a corresponding letter 13 places later in the alphabet a "wraparound" to the beginning of the alphabet should happen and that either lower or upper case latter should be chosen. Example: "x" -> "k" and "T" -> "G"
        - If one or more characters are provided which aren't in the lower or upper case English alphabet, that character stays unchanged.
        - Due to the nature of the English alphabet two successive ROT13 applications should result in the initially provided character list.
        - The length of the cipher compared to the plain text should stay the same.
-}

rot13:: [Char] -> [Char]
rot13 = map rot13Single

-- Calculates the ASCII-table difference between the first letter of either lower or upper case "A" and a given character.
getBaseOffset:: Char -> Char -> Int
getBaseOffset char base = ord char - ord base

-- Determines the offset of ciphered character relative towards the first letter of the alphabet. (Lower or Uppercase) 
-- Also takes into account any "rollover" from "Z" -> "A" which can happen while applying this cipher.
getRotatedOffset:: Char -> Char -> Int
getRotatedOffset char base = (getBaseOffset char base + 13) `mod` 26

-- Applies the ROT13 cipher rotation for lower or upper case characters.
-- The separation exists due to different ASCII table positioning of the lower and upper case alphabets in the ASCII table
rotateUpperCase:: Char -> Char
rotateUpperCase char = chr ( ord 'A' + getRotatedOffset char 'A')

rotateLowerCase:: Char -> Char
rotateLowerCase char = chr ( ord 'a' + getRotatedOffset char 'a')

-- Applies ROT13 cipher for a single character
rot13Single :: Char -> Char
rot13Single c
  | isAsciiLower c = rotateLowerCase c
  | isAsciiUpper c = rotateUpperCase c
  | otherwise      = c

-- QuickCheck property checks

prop_rot13_identity:: [Char] -> Bool
prop_rot13_identity chars = rot13 (rot13 chars) == chars

prop_rot13_length:: [Char] -> Bool
prop_rot13_length chars = length chars == length (rot13 chars)

getNonEnglishChars:: [Char] -> [Char] 
getNonEnglishChars = filter (not . isLetter)
prop_rot13_preserve_special_chars:: [Char] -> Bool
prop_rot13_preserve_special_chars chars = getNonEnglishChars chars == getNonEnglishChars (rot13 chars)

isCharLowerCase:: [Char] -> [Bool]
isCharLowerCase = map isLower
prop_rot13_case_preservation:: [Char] -> Bool
prop_rot13_case_preservation chars = isCharLowerCase chars == isCharLowerCase (rot13 chars)

isInLowerAlphabetHalf:: [Char] -> Bool
isInLowerAlphabetHalf [] = True
isInLowerAlphabetHalf (x:xs) = (('a' <= x && x < 'n') || ('A' <= x && x < 'N')) && isInLowerAlphabetHalf xs

filterTopAlphabetHalf:: [Char] -> [Char]
filterTopAlphabetHalf = filter (\x -> ('m' < x && x <= 'z' && isLower x) || ('M' < x && x <= 'Z' && isUpper x))

prop_rot13_rollover:: [Char] -> Bool
prop_rot13_rollover chars = isInLowerAlphabetHalf (rot13 (filterTopAlphabetHalf chars))

-- QuickCheck tests to check for each of the provided specification elements.
-- Since we are checking for all specifications, if every check is successful then it can be deemed correct.
main :: IO ()
main = do
    quickCheck prop_rot13_identity
    quickCheck prop_rot13_length
    quickCheck prop_rot13_preserve_special_chars
    quickCheck prop_rot13_case_preservation
    quickCheck prop_rot13_rollover