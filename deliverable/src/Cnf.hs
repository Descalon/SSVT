{--Exercise 4 (50 min)
    Whilest constraining ourselves to a conjunction and disjunction with cardinality 2, after reaching NNF the only thing left is apply the distributive property to reach CNF

    ran with the testCNF function. There seems to be some cases that cause the cnf function to never halt, but we ran out of time figuring out what's causing it.

    note: We tried to implement a function that could take disjunctions and conjunctions of any cardinality. But again; strech goal and we ran out of time.
--}

module Cnf where
    import Lib
    import Generator
    import Test.QuickCheck
    import Control.Monad

    cnf :: Form -> Form
    cnf f = while (not . isCnf) convertToCNF (nnf (arrowfree f))
    convertToCNF :: Form -> Form
    -- Keep literals as they are
    convertToCNF (Prop a) = Prop a
    convertToCNF (Neg (Prop a)) = Neg (Prop a)
    -- Distribute disjunctions over conjunctions
    convertToCNF (Dsj [a, (Cnj [b, c])]) = Cnj [(Dsj [(convertToCNF a), b]), (Dsj [(convertToCNF a), c])]
    convertToCNF (Dsj [(Cnj b), a]) = convertToCNF (Dsj [a, (Cnj b)])
    -- Analyse contents if outermost elements are disjunctions or conjunctions
    convertToCNF (Dsj [a, b]) = Dsj [(convertToCNF a), (convertToCNF b)]
    convertToCNF (Cnj [a, b]) = Cnj [(convertToCNF a), (convertToCNF b)]

    isCnf :: Form -> Bool
    isCnf (Prop x) = True
    isCnf (Neg (Prop x)) = True
    isCnf (Dsj xs) = (not (any isConjunction xs)) && (all (== True) (map isConjunction xs))
    isCnf (Cnj xs) = all (== True) (map isConjunction xs)
    isCnf _ = False

    isConjunction :: Form -> Bool
    isConjunction (Cnj xs) = True
    isConjunction _ = False

    testCnf = do
        f <- generate sizedForm
        putStrLn (show f)
        return (cnf f)
