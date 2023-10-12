module Exercise7 where

    -- Indication of time spent: 20 Minutes

    -- 1.) Example for when the symmetric closure of the transitive closure != transitive closure of the symmetric closure
    -- Relation:[ (1, 2), (2, 3), (3, 4)]
    -- Transitive Closure [(1,2),(2,3),(3,4),(1,3),(2,4),(1,4)]
    -- Symmetric Closure:  [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)]
    -- Symmetric closure of transitive closure: [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3),(1,3),(3,1),(2,4),(4,2),(1,4),(4,1)]
    -- Transitive closure of symmetric closure: [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3),(1,1),(1,3),(2,2),(2,4),(3,1),(3,3),(4,2),(4,4),(1,4),(4,1)]

    -- 1.1.) Why? Because the symmetric closure inverses all the tuples in a set. So, if you create a transitive closure
    -- over this symmetric relation, you will inevitabely run into the case that the "identity" tuples will be added during the 
    -- transitive closure. Transitive closure states that if a ~ b and b ~ c than a ~ c or if (a1, a2) ∈ R and (a2, a3) ∈ R, then (a1, a3) ∈ R. 
    -- So if you have a tuple (1,0) create the inverse (0,1) you will have in the transitive closure the (1,1) tuple.

    -- 2.) Examples for then the symmetric closure of the transitive closure == transitive closure of the symmetric closure
    -- 2.1) If you have an empty set, then the symmetric closure will be an empty set but also the transitive closure.
    -- 2.2) If you have a set of solely identities e.g. [(1,1),(2,2)], then you are going to have the case symmetric closure of the transitive closure == transitive closure of the symmetric closure
    -- because a == b and b == c and a == c ("looping")

    -- Conclusion: Both cases are possible. 
    -- Create a quickcheck test with relation stuff and falsify 