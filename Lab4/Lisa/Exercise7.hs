module Exercise7 where
    -- Relation:[ (1, 2), (2, 3), (3, 4)]
    -- Transitive Closure [(1,2),(2,3),(3,4),(1,3),(2,4),(1,4)]
    -- Symmetric Closure:  [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)]

    -- Symmetric closure of transitive closure: [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3),(1,3),(3,1),(2,4),(4,2),(1,4),(4,1)]
    -- Transitive closure of symmetric closure: [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3),(1,1),(1,3),(2,2),(2,4),(3,1),(3,3),(4,2),(4,4),(1,4),(4,1)]

    -- They are not the same? 
    
    -- Haha, it depends on the domain which you are using it. 

    -- It depends on the set you are using.

    -- e.g. For the relation: [(1,2),(2,3)] they are indeed the same. 

