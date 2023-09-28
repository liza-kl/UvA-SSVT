
module Exercise1 where
import LTS
import Data.List
import Test.QuickCheck

{-- 
Time Spent: 30 Minutes, mainly documentation
--}

{-- 
List of Factors that make an IOLTS invalid: 

- #1 Factor The IOLTS tuple doesn't have 5 values -- Checked by compiler
- #2 Factor The joint of L_i (input set) and L_o (output set) is not an empty set 
- #3 Factor L_i and L_o are not countable -- Dunno how to implement
- #4 Factor Q (the set of states) is an empty set 
- #5 Factor q0 (inital state) ist not an element of Q 
- #6 Factor tau is in the L_i set
- #7 Factor Transition T is not a subset of the cartesian does not satisfy the transition relation 
- #8 Factor Delta Transitions are not returning the previous state (-> not "looping")
--}

createCartesian :: [State] -> [Label] -> [(State, Label, State)]
createCartesian setState inputValues =  [(preState, transitionLabel, afterState) | preState <- setState, transitionLabel <- inputValues ,afterState <- setState ]


validateLTS :: IOLTS -> Bool
validateLTS ([], _, _, _, _) = False -- if your initial set is empty, it is not an LTS / IOLTS , but not checking for empty inputs as this can be the case according to definition
validateLTS (setState, inputValues, outputValues, labeledTransitions, initialState)
    | tau `elem` inputValues = False
    | not (null (inputValues `intersect` outputValues)) = False -- Check if list is empty
    | initialState `notElem` setState = False
    | not (all (`elem` createCartesian setState (inputValues ++ [tau] ++ outputValues)) labeledTransitions) = False
    | otherwise = True

-- #2 Factor
prop_interSectionOfInputOutputIsEmpty :: IOLTS -> Bool
prop_interSectionOfInputOutputIsEmpty (_, inputValues, outputValues,_,_) = null (inputValues `intersect` outputValues)

-- #4 Factor
prop_initialSetNotEmpty :: IOLTS -> Bool
prop_initialSetNotEmpty (setOfStates, _,_,_,_) = not (null setOfStates)

-- #6 Factor
prop_tauNotInInputSet :: IOLTS -> Bool
prop_tauNotInInputSet (_, inputValues,_,_,_) = tau `notElem` inputValues

-- #5 Factor
prop_initialStateInStateSet :: IOLTS -> Bool
prop_initialStateInStateSet (stateSet, _,_,_,initialValue) = initialValue `elem` stateSet

-- #7 Factor
-- Every transition tuple must conform the transition definitio

prop_cartesianRelationInTransition :: IOLTS -> Bool
prop_cartesianRelationInTransition (stateSet, inputValues, outputValues,labeledTransitions,_) =
   all (`elem` createCartesian stateSet (inputValues ++ [tau] ++ [delta] ++ outputValues)) labeledTransitions -- Adding delta for the idle state

-- #8 Factor
prop_deltaBehavior :: IOLTS -> Bool
prop_deltaBehavior (_,_,_,[],_) = True
prop_deltaBehavior (_,[],_,_,_) = True
prop_deltaBehavior (_,_,[],_,_) = True
prop_deltaBehavior (_, _, _,[(pre,trans,post)],_) = (trans /= delta) || (pre == post)
prop_deltaBehavior _ = False

{-- 
Concise Test Report
Validated agains the coffee examples in the LTS module as no QuickCheck tests are asked for.
First, we didn't udnerstand why the prop_cartesianRelationInTransition returns false but validates correctly?
--}
main :: IO ()
main =  do
        let ltsImplementations = [
                                counterImpl,counterModel,
                                coffeeImpl1, coffeeModel1,
                                coffeeImpl2, coffeeModel2,
                                coffeeImpl3,coffeeModel3,
                                coffeeImpl4,coffeeModel4,
                                coffeeImpl5,coffeeModel5,
                                coffeeImpl6,coffeeModel6,
                                tretmanK2,tretmanK3,
                                tretmanI1,tretmanI2,tretmanI3,tretmanI4,
                                tretmanS1,tretmanS2,tretmanS3,tretmanS4,
                                tretmanR1,tretmanR2]
        print (all validateLTS ltsImplementations)