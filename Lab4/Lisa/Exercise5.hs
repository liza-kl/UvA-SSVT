import Data.List
import qualified Data.Set as Set

type Rel a = [(a, a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [(x, z) | (x, y1) <- r, (y2, z) <- s, y1 == y2]

-- Compute the full transitive closure using Warshall's algorithm
trClos :: Ord a => Rel a -> Rel a
trClos relation = closure relation
  where
    closure r
      | r == r2 = r
      | otherwise = closure r2
      where r2 = nub (r ++ (r @@ r))

main :: IO ()
main = do
    let inputRelation = [(1, 2), (2, 3), (3, 4)]
    let result = trClos inputRelation
    print result
