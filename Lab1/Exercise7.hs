
module Exercise7 where
import SetOrd 
import Test.QuickCheck
import System.Random
import Lecture2
import System.IO.Unsafe

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

genRandomInteger:: Int -> Int
genRandomInteger num = unsafePerformIO(Lecture2.getRandomInt num)

instance Arbitrary Form where
    arbitrary = do
        f1 <- arbitrary
        f2 <- arbitrary
        elements [ Prop f1
                , Prop (genRandomInteger 5)-- TODO SOMEHOW GENERATE A RANDOM NUMBER IN THERE
                 , Neg (Prop f1)
                 , Cnj [Prop f1, Prop f2]
                 , Dsj [Prop f1, Prop f2]
                 , Impl (Prop f1) (Prop f2)
                 , Equiv (Prop f1) (Prop f2)
                 ]



{-- nsub :: Form -> Int 

 7.1 a) --}