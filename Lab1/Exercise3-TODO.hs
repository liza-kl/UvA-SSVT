{-- TODO --}

import Data.List
import Data.Char
import Data.Maybe
import Test.QuickCheck


infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

{-- 
Rank the 4 expressions, not only bits

(\ x -> even x && x > 3) or even
(\ x -> even x || x > 3) or even
(\ x -> (even x && x > 3) || even x) or even
even or (\ x -> (even x && x > 3) || even x)
--}