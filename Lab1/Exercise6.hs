module Exercise6 where

import Data.List

-- Time spent: 3 hours, due to testing

-- Simplified conversion method (since we don't have all logical elements are defined in the "Form" type)
-- Source: Lecture Slides (Lecture 3)

{-
    Procedure: 
        1) Remove implications and equivalences.
            I.E: Replace (P Impl Q) with Neg(P or Q)
            And replace (P Equiv Q) with (P or (Neg Q)) and ((Neg P) or Q)
        2) Move Neg's as much inside as possible using De Morgan's laws.
        3) Distribute ORs inwards over ANDs by replacing... (P or (Q and R)) with ((P or Q) and (P or R))
-}


type Name = Int
data Form = Prop Name
          | Neg Form
          | Cnj [Form]
          | Dsj [Form]
          | Impl Form Form
          | Equiv Form Form
          deriving (Eq,Ord)

cnf :: Form -> Form
cnf (Neg (Neg p))         = cnf p                                                        -- replace ¬¬P with P
cnf (Impl p q)            = cnf (Dsj [Neg (cnf p), cnf q])                               -- replace P → Q with ¬ P ∨ Q 
cnf (Equiv p q)           = Cnj [Dsj [cnf p, Neg (cnf q)], Dsj [Neg (cnf p), cnf q]]     -- replace P ↔ Q with (P∨¬Q)∧(¬P∨Q)
cnf (Neg (Dsj [p, q]))    = Cnj [cnf (Neg (cnf p)), cnf(Neg (cnf q))]                    -- replace ¬(P∨Q) with (¬P)∧(¬Q)
cnf (Neg (Cnj [p, q]))    = Dsj [cnf (Neg (cnf p)), cnf(Neg (cnf q))]                    -- replace ¬(P∧Q) with (¬P)∨(¬Q)
cnf (Dsj [p, Cnj [q, r]]) = Cnj [cnf (Dsj [cnf p, cnf q]), cnf (Dsj [cnf p, cnf r])]     -- replace P∨(Q∧R) with (P∨Q)∧(P∨R)
cnf (Dsj [Cnj [p, q], r]) = Cnj [cnf (Dsj [cnf p, cnf r]), cnf (Dsj [cnf q, cnf r])]     -- replace P∨(Q∧R) with (P∨Q)∧(P∨R)
cnf (Dsj [p, Dsj [q, r]]) = Dsj [cnf p, cnf q, cnf r]                                    -- remove inner disjunction
cnf (Dsj [Dsj [q, r], p]) = Dsj [cnf p, cnf q, cnf r]                                    -- remove inner disjunction
cnf (Cnj p)               = Cnj (map cnf p)                                              -- cnf conjunction
cnf (Dsj p)               = Dsj (map cnf p)                                              -- cnf disjunction
cnf p                     = p                                                            -- other


instance Show Form where
    show (Prop name) = "Prop " ++ show name
    show (Neg form) = "Neg (" ++ show form ++ ")"
    show (Cnj forms) = "Cnj " ++ show forms
    show (Dsj forms) = "Dsj " ++ show forms
    show (Impl p q) = "Impl (" ++ show p ++ ") (" ++ show q ++ ")"
    show (Equiv p q) = "Equiv (" ++ show p ++ ") (" ++ show q ++ ")"


main :: IO ()
main = do
     let booleanFormula = Dsj [Cnj [Prop 1, Prop 0], Impl (Prop 0) (Prop 1)] 
     putStrLn ("Boolean Formula: " ++ show booleanFormula)
     putStrLn ("CNF Formula: " ++ show (cnf booleanFormula))