module Propositions where
    import Lib

    contradiction :: Form -> Bool
    contradiction f = all (\ v -> not (evl v f)) (allVals f)

    tautology :: Form -> Bool
    tautology f = all (\ v -> evl v f) (allVals f)

    entails :: Form -> Form -> Bool
    entails a b = all (\ v -> evl v a) vals
                where
                    vals = filter (\ v -> evl v b) (allVals b)

    equiv :: Form -> Form -> Bool
    equiv a b = entails a b && entails b a

    contradictionCheck = contradiction $ Cnj [p, Neg(p)]
    tautologyCheck = tautology (Dsj [p, Neg(p)])
    entailsCheck = entails (Dsj [p,q]) (Cnj [p,q])
    equivalanceCheck = Cnj [Neg(p), Neg(q)] `equiv` Neg (Dsj [p, q])