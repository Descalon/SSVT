module Cnf where
    import Lib

    toNnf :: Form -> Form
    toNnf = arrowfree . nnf

    u = Prop 4
    v = Prop 5

    val1 = Dsj [p, Cnj [q,r]]
    val1D = Cnj [Dsj [p, q], Dsj[p,r]]

    val2 = Dsj [p, q, Cnj[r,u,v]]
    val2D = Cnj [Dsj[p,q,r], Dsj[p,q,u], Dsj[p,q,v]]

    val3 = Dsj[Cnj [p,q], Cnj[r,u]]
    val3D = Cnj[Dsj[p,r], Dsj[q,r], Dsj[p,u], Dsj[q,u]]

    dsjDistribution :: Form -> Form
    dsjDistribution (Dsj [x, Cnj fs]) = Cnj $ map (\f -> Dsj [x, dsjDistribution f]) fs
    dsjDistribution (Dsj [Cnj fs, x]) = Cnj $ map (\f -> Dsj [x, dsjDistribution f]) fs
    dsjDistribution x = x

    cnf :: Form -> Form
    cnf (Prop a) = Prop a
    cnf (Neg (Prop a)) = Neg (Prop a)
    cnf (Cnj xs) = Cnj (map (cnf) xs)
    cnf (Dsj xs) = cnf $ dsjDistribution (Dsj xs)