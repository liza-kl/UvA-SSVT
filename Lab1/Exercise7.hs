-- Time Spent: 5.1 60 Minutes 5.2. 60 Minutes,
-- was the last and hardest exercise for us.


module Exercise7 where
import SetOrd
import Test.QuickCheck
import System.Random
import Lecture2

{-- Already provided code from the exercise --}
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

{-- 
    Code was taken and modified from Chapter 7.7 Haskell Road to Logic
    This function counts the conjunctions

    If Atoms inside conjunctions are the same there is one less subformula when they are different.
    This is due to not including the simplest subformula for f2 since that is
    equivalent to the simplest subformula of f1.
    -> We needed to add this, because otherwise our prop_lengthOfSet failed.

--}
ccount :: Form -> Int
ccount (Prop n) = 0 -- a single property is atomic, so the length is zero
ccount (Cnj [f1,f2]) = if f1 == f2 then ccount f1 + ccount f2 else 1 + ccount f1 + ccount f2 -- With the following yo
--  are counting the subformulas, the + 1 is the final conjunction that also needs to be added
ccount (Dsj [f1,f2]) = if f1 == f2 then ccount f1 + ccount f2 else 1 + ccount f1 + ccount f2 -- "see above"
ccount (Impl f1 f2) = if f1 == f2 then ccount f1 + ccount f2 else 1 + ccount f1 + ccount f2 -- "see above"
ccount (Equiv f1 f2) = if f1 == f2 then ccount f1 + ccount f2 else 1 + ccount f1 + ccount f2 -- "see above"
ccount (Neg f) = 1 + ccount f -- "see above"

{-- This piece counts the atomic properties in a logical proposition, it is needed, because the ccount does
not count them – but they are still a sub formula. --}
acount :: Form -> Int
acount (Prop n) = 1 -- "when the base case of atomic value is reached, add 1"
acount (Cnj [f1,f2]) = acount f1 + acount f2
acount (Dsj [f1,f2]) = acount f1 + acount f2
acount (Impl f1 f2) = acount f1 + acount f2
acount (Equiv f1 f2) = acount f1 + acount f2
acount (Neg f) = acount f

{-- The length of subformulas is the sum of atomic properties and the number of conjuctions,
this can be proven by induction. Chapter 7.7 in the Haskell Road to Logic 
--}
nsub :: Form -> Int
nsub form = ccount form + acount form

{-- Generator which generates random Forms for the testing --}
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
                ,Impl (Prop f1) (Prop f2)
                ,Equiv (Prop f1) (Prop f2)
                 ]

{-- Helper Function to get the size of a set --}
getLengthOfSet :: Set a -> Int
getLengthOfSet (Set []) = 0
getLengthOfSet (Set (x:xs)) = 1 + getLengthOfSet (Set xs)

{-- Generator function for the following tests --}
formsGenerator :: Gen Form
formsGenerator = arbitrary :: Gen Form

-- QuickCheck Properties

{-- 
Properties for 7.1
1.) The length of the set, which is returned by the sub a function should equals the
length of the nsub function (as this is proven by induction).

For counting the elements you could have probably used some form of the inclusion-exclusion principle,
as we are dealing with sets.

2.) The other property is, that the set of the subformulas should include the original formula 
(as this is a subformula as well.

3.) A very specific property is that the length of a single atom should equal the set of the prop

--}

-- Code for 1.) property
prop_lengthOfSet :: Form -> Bool 
prop_lengthOfSet form = nsub form == getLengthOfSet (sub form)

-- Code for 2.) property
prop_formIsInSubForm :: Form -> Bool
prop_formIsInSubForm form = inSet form (sub form)

-- Code for 3.) property
prop_SingleAtom :: Int -> Property
prop_SingleAtom x = property (sub (Prop x) == Set [Prop x])

{-- Properties for 7.2
1.) Testing the property for a single atom that it is one
Otherwise it is proven by induction.
--}

prop_SingleAtomAmount :: Int -> Property
prop_SingleAtomAmount x = property ( nsub (Prop x) == 1)

main :: IO Result
main = do
    quickCheck prop_SingleAtom
    quickCheck prop_SingleAtomAmount
    quickCheckResult $ forAll formsGenerator prop_formIsInSubForm
    quickCheckResult $ forAll formsGenerator prop_lengthOfSet