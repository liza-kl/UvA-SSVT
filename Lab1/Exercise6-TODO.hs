module Exercise6 where

{-- TODO --}

-- Conversion method https://en.wikipedia.org/wiki/Conjunctive_normal_form

type Name = Int

data Form = Prop Name
    | Neg Form
    | Cnj [Form]
    | Dsj [Form]
    | Impl Form Form
    | Equiv Form Form
    deriving (Eq,Ord)

-- cnf::Form -> Form

{-

    Procedure: 
        1) Remove implications and equivalences.
            I.E: Replace (P Impl Q) with Neg(P or Q)
            And replace (P Equiv Q) with (P or (Neg Q)) and ((Neg P) or Q)
        2) Move Neg's as much inside as possible using De Morgan's laws.
        3) Unsure if this applies here. Don't think so. Drop universal Quantifiers. (∀)
        4) Distribute ORs inwards over ANDs by replacing... (P or (Q and R)) with ((P or Q) and (P or R))

-}