
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

instance Arbitrary Form where
    arbitrary = do
        f1 <- arbitrary
        f2 <- arbitrary
        rndNum <- choose (1,15)
        elements [ Prop f1
                 , Prop rndNum
                 , Neg (Prop f1)
                 , Cnj [Prop f1, Prop f2]
                 , Dsj [Prop f1, Prop f2]
                 , Impl (Prop f1) (Prop f2)
                 , Equiv (Prop f1) (Prop f2)
                 ]



{-- nsub :: Form -> Int 

 7.1 a) You can test it with the inclusive / exclusive principle 
 
 --}

ccount :: Form -> Int
ccount (Prop n) = 0
ccount (Cnj [f1,f2]) = 1 + ccount f1 + ccount f2
ccount (Dsj [f1,f2]) = 1 + ccount f1 + ccount f2
ccount (Impl f1 f2) = 1 + ccount f1 + ccount f2
ccount (Equiv f1 f2) = 1 + ccount f1 + ccount f2
ccount (Neg f) = 1 + ccount f

acount :: Form -> Int
acount (Prop n) = 1
acount (Cnj [f1,f2]) = acount f1 + acount f2
acount (Dsj [f1,f2]) = acount f1 + acount f2
acount (Impl f1 f2) = acount f1 + acount f2
acount (Equiv f1 f2) = acount f1 + acount f2
acount (Neg f) = acount f

nsub :: Form -> Int
nsub form = (ccount form) + (acount form)

-- QuickCheck Properties

propSingleAtom :: Int -> Property
propSingleAtom x = property (sub (Prop x) == Set [Prop x])

propSingleAtomAmount :: Int -> Property
propSingleAtomAmount x = property( nsub (Prop x) == 1)

main :: IO ()
main = do
    quickCheck propSingleAtom
    quickCheck propSingleAtomAmount