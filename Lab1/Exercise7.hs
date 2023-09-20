
module Exercise7 where
import SetOrd 

type Name = Int

data Form = Prop Name 
    | Neg  Form 
    | Cnj [Form] 
    | Dsj [Form] 
    | Impl Form Form 
    | Equiv Form Form 
    deriving (Eq,Ord, Show)

sub :: Form -> Set Form 
sub (Prop x) = Set [Prop x] 
sub (Neg f) = unionSet (Set [Neg f]) (sub f) 
sub f@(Cnj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2) 
sub f@(Dsj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2) 
sub f@(Impl f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2) 
sub f@(Equiv f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)

{-- nsub :: Form -> Int 

 7.1 a) --}