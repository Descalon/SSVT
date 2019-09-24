module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import SetOrd

-- Time spent - slightly under an hour
-- not even a single eval should succeed
contradiction :: Form -> Bool
contradiction = not . satisfiable

-- they should all succeed, therefore, loop over all possiblities an all should be true
tautology :: Form -> Bool
tautology f = all (\v -> evl v f) (allVals f)

-- For all value combinations for form1, whatever form 1 returns should be valid for form 2
entails :: Form -> Form -> Bool
entails f1 f2 = all (\v -> if (evl v f1) then (evl v f2) else True) (genVals (propNames f1 ++ propNames f2))

-- Just like entails, but vice versa as well
equiv :: Form -> Form -> Bool
equiv f1 f2 = entails f1 f2 && entails f2 f1

-- assignment 3
-- 3 hours +-
-- Resources used:
-- Demorgan's law / cnf wikipedia pages

ex1 = (Neg (Dsj [Prop 1, Neg(Prop 2)]))

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


-- assignment 5

sub :: Form -> Set Form
sub (Prop x) = Set [Prop x]
sub (Neg f) = unionSet (Set [Neg f]) (sub f)
sub f@(Cnj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Dsj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Impl f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Equiv f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)


-- simple method to count the props in a form
propCount :: Form -> Integer
propCount (Prop x) = 1
propCount (Neg x) = 1 + propCount x
propCount (Dsj [a, b]) = 1 + propCount a + propCount b
propCount (Cnj [a, b]) = 1 + propCount a + propCount b
propCount (Impl f1 f2) = 1 + propCount f1 + propCount f2
propCount (Equiv e1 e2) = 1 + propCount e1 + propCount e2


-- method to check whether all formulas from a form are found within the result of sub <form>
allSet :: Form -> Set Form -> Bool
allSet (Prop x) s = inSet (Prop x) s
allSet (Neg x) s = allSet x s
allSet (Dsj [a, b]) s = inSet (Dsj [a, b]) s && allSet a s && allSet b s
allSet (Cnj [a, b]) s = inSet (Cnj [a, b]) s && allSet a s && allSet b s
allSet (Impl f1 f2) s = inSet (Impl f1 f2) s && allSet f1 s && allSet f2 s && inSet f1 s && inSet f2 s
allSet (Equiv f1 f2) s = inSet (Equiv f1 f2) s && allSet f1 s && allSet f2 s && inSet f1 s && inSet f2 s
