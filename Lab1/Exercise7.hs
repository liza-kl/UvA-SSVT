
module Exercise7 where
import SetOrd
import Test.QuickCheck
import System.Random
import Lecture2

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

{-- Generator which generates random Forms --}
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

{-- 

 7.1 a) You can test it with the inclusive / exclusive principle 
 
 --}

{-- Code was taken and modified from Chapter 7.7 in the Haskel Road to Logic book --}
{-- This function counts the conjunctions --}
ccount :: Form -> Int
ccount (Prop n) = 0 -- a single property is atomic, so the length is zero
ccount (Cnj [f1,f2]) = 1 + ccount f1 + ccount f2 -- With the following yo
--  are counting the subformulas, the + 1 is the final conjunction that also needs to be added
ccount (Dsj [f1,f2]) = 1 + ccount f1 + ccount f2 -- "see above"
ccount (Impl f1 f2) = 1 + ccount f1 + ccount f2 -- "see above"
ccount (Equiv f1 f2) = 1 + ccount f1 + ccount f2 -- "see above"
ccount (Neg f) = 1 + ccount f -- "see above"

acount :: Form -> Int
acount (Prop n) = 1 -- "when the base case of atomic value is reached, add 1"
acount (Cnj [f1,f2]) = acount f1 + acount f2
acount (Dsj [f1,f2]) = acount f1 + acount f2
acount (Impl f1 f2) = acount f1 + acount f2
acount (Equiv f1 f2) = acount f1 + acount f2
acount (Neg f) = acount f

{-- The length of subformulas is the sum of atomic properties and the number of conjuctions,
this can be proven by induction. Chapter 7.7 in the Haskel Road to Logic 
--}
nsub :: Form -> Int
nsub form = ccount form + acount form

-- QuickCheck Properties

formsGenerator :: Gen Form
formsGenerator = arbitrary :: Gen Form

prop_formIsInSubForm :: Form -> Bool
prop_formIsInSubForm form = inSet form (sub form)

prop_SingleAtom :: Int -> Property
prop_SingleAtom x = property (sub (Prop x) == Set [Prop x])

prop_SingleAtomAmount :: Int -> Property
prop_SingleAtomAmount x = property ( nsub (Prop x) == 1)

main :: IO Result
main = do
    quickCheck prop_SingleAtom
    quickCheck prop_SingleAtomAmount
    quickCheckResult $ forAll formsGenerator prop_formIsInSubForm
