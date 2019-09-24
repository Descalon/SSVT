module Cnf where
    import Lib

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