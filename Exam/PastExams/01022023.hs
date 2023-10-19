module Exam01022023 where 

-- Property t0 should be x = 0 then it should return x 0 
-- Second Property: Implement equivalence in ahskell 
-- Equivalence stuff in mutation testing 

corona4 r s x0 t = (!! t) $ zipWith (+) (iterate (+s) 0) (iterate (*r) x0)