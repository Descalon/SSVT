import Lecture3
import SetOrd


-- satisfiable :: Form -> Bool
-- satisfiable f = any (\ v -> evl v f) (allVals f)

formule = head (parse ("(*(-+(1 2) 3)==>4)"))

{-- Assignment 1
  Indication of time spent: 2h
  Description of your method of checking the definitions:
  Implementation: Below
--}
contradiction :: Form -> Bool
contradiction = not . satisfiable

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

entails :: Form -> Form -> Bool
entails f1 f2 = all (\v -> if (evl v f1) then (evl v f2) else True) (genVals (propNames f1 ++ propNames f2))

equivalence :: Form -> Form -> Bool
equivalence f1 f2 = entails f1 f2 && entails f2 f1

{-- Assignment 2
  Indication of time spent: (Start : 11:30)
  Test report describing the test method used and the outcome of the test
--}


{-- Assignment 3
  Indication of time spent: 3h
  Documentation: cnf coverts a Haskell generated Form into the Conjuctive Normalform,
  which constists only of the following grammar:

          L ::= p|¬p
          D ::= L|L∨D
          C ::= D|D∧C

  Usage is: cnf <form>, function of each step is implemented in definition.

  Implementation: Below
--}
ex1 = (Neg (Dsj [Prop 1, Neg(Prop 2)]))
ex2 = (Impl (Cnj [Neg (Dsj [Prop 1, Prop 2]), Prop 3]) (Prop 4))

cnf :: Form -> Form
cnf f = convertToCNF (nnf (arrowfree f))
convertToCNF :: Form -> Form
-- Remove Brackets from Literals
convertToCNF (Prop a) = Prop a
convertToCNF (Neg (Prop a)) = Neg (Prop a)
-- Remove Double Negatives
convertToCNF (Neg (Neg a)) = convertToCNF (nnf (Neg (Neg a)))
-- Distribute disjunctions over conjunctions
convertToCNF (Dsj [a, (Cnj [b, c])]) = Cnj [(Dsj [(convertToCNF a), b]), (Dsj [(convertToCNF a), c])]
-- Distribute conjunctions over disjunctions
convertToCNF (Dsj [(Cnj b), a]) = convertToCNF (Dsj [a, (Cnj b)])
-- Analyse contents if outermost elements are disjunctions or conjunctions
convertToCNF (Dsj [a, b]) = Dsj [(convertToCNF a), (convertToCNF b)]
convertToCNF (Cnj [a, b]) = Cnj [(convertToCNF a), (convertToCNF b)]


{-- Assignment 5
  Indication of time spent: (Start : 15:19) (End: 18:18)
--}
 {---
    5.1
 -}
sub :: Form -> Set Form
sub (Prop x) = Set [Prop x]
sub (Neg f) = unionSet (Set [Neg f]) (sub f)
sub f@(Cnj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Dsj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Impl f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Equiv f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)

-- Returns cardinality of a form whilst discarding duplicates
formSize :: Form -> Int
formSize f = formSize' f + (length (propNames f))
              where
                formSize' (Prop x) = 0
                formSize' (Neg x) = formSize' x + 1
                formSize' (Dsj [x, y]) = formSize' x + formSize' y + 1
                formSize' (Cnj [x, y]) = formSize' x + formSize' y + 1
                formSize' (Impl x y) = formSize' x + formSize' y + 1
                formSize' (Equiv x y) = formSize' x + formSize' y + 1

-- Returns cardinality of a set of subformulas
setSize :: Set a -> Int
setSize f = setSize' f 0
              where
                setSize' (Set []) c = c;
                setSize' (Set (x:xs)) c = setSize' (Set xs) (c+1);

-- Tests the propery of equal cardinality between form and set of subformulas
propertyEqualLengthTest :: Form -> Bool
propertyEqualLengthTest f = (formSize f) == (setSize (sub f))
