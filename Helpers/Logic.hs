module Logic where

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = not p || q